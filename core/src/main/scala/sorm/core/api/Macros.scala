package sorm.core.api

object Macros {

  import reflect.macros.Context

  /**
   * Expands an `Entity => Value` function from a `ref` parameter to a value
   * `SubRef[ Entity, Value ]`, then passes it to an overloaded version of
   * the macro-triggering method.
   *
   * @example
   *   {{{
   *     .equals( _.genre.name, "Jazz" )
   *   }}}
   *   gets expanded into
   *   {{{
   *     .equals( SubRef( <Type of Genre>,
   *                      List( <Symbol of "genre" field of type Genre>,
   *                            <Symbol of "name" field of a type of field
   *                              "genre" of the Genre type> ),
   *              "Jazz" )
   *   }}}
   *
   * SubRef's context type value should be generated from the passed in `Entity`
   * type-parameter.
   */
  def equals
    [ driver,
      entity : c.WeakTypeTag,
      value : c.WeakTypeTag,
      input ]
    ( c : Context )
    ( ref : c.Expr[ entity => value ],
      value : c.Expr[ value ] )
    : c.Expr[ WhereComposition[ driver, entity, (value, input) ] ]
    = {

    import c.universe._

    val Entity = TypeTree(weakTypeOf[entity])
    val Value = TypeTree(weakTypeOf[value])
    def ru = treeBuild.mkRuntimeUniverseRef
    def reifyType(tpe: Type) = c.reifyType(ru, EmptyTree, tpe, concrete = true)

    object PropertyPath {
      def unapply(tree: Tree): Option[List[RefTree]] = tree match {
        case tree @ Ident(name) if name.isTermName => Some(List(tree))
        case tree @ Select(prefix, name) if name.isTermName => unapply(prefix).map(_ :+ tree)
        case _ => None
      }
    }

    val reifiedSymbols = ref.tree match {
      case Function(List(ValDef(_, paramName,_, _)), PropertyPath(target :: props)) if target.name == paramName =>
        def loop(tpe: Type, props: List[RefTree], syms: List[Symbol]): List[Symbol] = props match {
          case prop :: others =>
            val sym = tpe.members.collect{ case m: MethodSymbol if m.name == prop.name && m.isGetter => m }.headOption
            sym match {
              case Some(sym) => loop(sym.returnType, others, syms :+ sym)
              // FIXME: any additional validation? does the owner have to be a case class? a top-level case class?
              case None => c.abort(prop.pos, s"only getters are supported in equals clauses")
            }
          case Nil => syms
        }
        val symbols = loop(Entity.tpe, props, Nil)
        symbols.map(symbol => {
          val reifiedOwner = Select(Select(reifyType(symbol.owner.asType.toType), newTermName("tpe")), newTermName("typeSymbol"))
          val selectTerm = Select(Select(ru, newTermName("build")), newTermName("selectTerm"))
          Apply(selectTerm, List(reifiedOwner, Literal(Constant(symbol.name.toString))))
        })
      case _ =>
        println(showRaw(ref.tree))
        c.abort(c.enclosingPosition, s"only property paths are supported in equals clauses")
    }

    val SubRef = selectTerm(c)("sorm.core.FieldRef")
    val contextType = Select(reifyType(weakTypeOf[entity]), newTermName("tpe"))
    val subFieldSymbols = Apply(Ident(newTermName("List")), reifiedSymbols)
    val subRef = Apply(TypeApply(SubRef, List(Entity, Value)), List(contextType, subFieldSymbols))

    c.Expr(Apply(Select(c.prefix.tree, newTermName("equals")), List(subRef, value.tree)))
  }

  def uniqueKey
    [ entity : c.WeakTypeTag, fields : c.WeakTypeTag ]
    ( c : Context )
    ( f : c.Expr[ entity => fields ] )
    : c.Expr[ Key.Unique[ entity, fields ] ]
    = ???

  def entity
    [ a : c.WeakTypeTag ]
    ( c : Context )
    ( keys : c.Expr[ Set[ Key[ a, Any ] ] ] )
    : c.Expr[ Entity[ a ] ]
    = {

    import c.universe._
    import Flag._

    val T = weakTypeOf[a]
    val fields = T.declarations.collect{ case x: MethodSymbol if x.isParamAccessor => x }.toList

    def mkAnon(parents: List[Tree], methods: List[Tree])  = Block(List(
      ClassDef(Modifiers(FINAL), newTypeName("$anon"), Nil, Template(parents, emptyValDef, methods))),
      Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List()))
    def mkSuperRef() = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)
    def mkCtor(superArgs: List[Tree]) = DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(List(Apply(mkSuperRef(), superArgs)), Literal(Constant(()))))
    def mkParam(name: String, tpe: Tree) = ValDef(Modifiers(PARAM), newTermName(name), tpe, EmptyTree)
    def mkDefaultParam(name: String, tpe: Tree, rhs: Tree) = ValDef(Modifiers(PARAM | DEFAULTPARAM), newTermName(name), tpe, rhs)
    def mkSormRef(name: Name) = Select(selectTerm(c)("sorm.core.api"), name)

    val Entity = mkSormRef(newTypeName("Entity"))
    val Persisted = mkSormRef(newTypeName("Persisted"))
    val IndexOutOfBoundsException = Ident(newTypeName("IndexOutOfBoundsException"))
    val Key = mkSormRef(newTypeName("Key"))
    val Set = Ident(newTypeName("Set"))
    val Long = Ident(newTypeName("Long"))
    val String = Ident(newTypeName("String"))
    val Int = Ident(newTypeName("Int"))
    val Any = Ident(newTypeName("Any"))
    val setOfKeysType = AppliedTypeTree(Set, List(AppliedTypeTree(Key, List(TypeTree(T), Ident(newTypeName("Any"))))))
    val TWithPersisted = CompoundTypeTree(Template(List(TypeTree(T), Persisted), emptyValDef, Nil))

    val copyParams = fields.map(field => mkDefaultParam(field.name.toString, TypeTree(field.typeSignature), Select(Ident(newTermName("value")), field.name)))
    val copyCtor = Apply(Select(New(TypeTree(T)), nme.CONSTRUCTOR), fields.map(field => Ident(field.name)))
    val copyBody = Apply(Ident(newTermName("mixinPersisted")), List(copyCtor, Ident(newTermName("id"))))
    val copy = DefDef(Modifiers(OVERRIDE), newTermName("copy"), Nil, List(copyParams), TWithPersisted, copyBody)

    val productElementZero = CaseDef(Literal(Constant(0)), EmptyTree, Ident(newTermName("id")))
    val productElementFields = fields.zipWithIndex.map{ case (field, i) => CaseDef(Literal(Constant(i + 1)), EmptyTree, Ident(field.name)) }
    val productElementFallback = CaseDef(Ident(nme.WILDCARD), EmptyTree, Throw(Apply(Select(New(IndexOutOfBoundsException), nme.CONSTRUCTOR), List(Select(Ident(newTermName("n")), newTermName("toString"))))))
    val productElementBody = Match(Ident(newTermName("n")), productElementZero +: productElementFields :+ productElementFallback)
    val productElement = DefDef(Modifiers(OVERRIDE), newTermName("productElement"), Nil, List(List(mkParam("n", Int))), Any, productElementBody)

    val equalsId = Apply(Select(Ident(newTermName("id")), newTermName("$eq$eq")), List(Select(Ident(newTermName("that")), newTermName("id"))))
    val equalsSuper = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), newTermName("equals")), List(Ident(newTermName("that"))))
    val equalsIdAndSuper = Apply(Select(equalsId, newTermName("$amp$amp")), List(equalsSuper))
    val equalsPersisted = CaseDef(Bind(newTermName("that"), Typed(Ident(nme.WILDCARD), Persisted)), EmptyTree, equalsIdAndSuper)
    val equalsFallback = CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(false)))
    val equalsBody = Match(Ident(newTermName("that")), List(equalsPersisted, equalsFallback))
    val equals = DefDef(Modifiers(OVERRIDE), newTermName("equals"), Nil, List(List(mkParam("that", Any))), TypeTree(), equalsBody)

    val mixinCtor = mkCtor(fields.map(field => Select(Ident(newTermName("value")), field.name)))
    val id = ValDef(NoMods, newTermName("id"), TypeTree(), Ident(newTermName("idValue")))
    val productArity = DefDef(Modifiers(OVERRIDE), newTermName("productArity"), Nil, Nil, TypeTree(), Literal(Constant(fields.length + 1)))
    val mixinPersistedBody = mkAnon(List(TypeTree(T), Persisted), List(mixinCtor, id, copy, productElement, productArity, equals))

    val entityCtor = mkCtor(Nil)
    val keysField = ValDef(NoMods, newTermName("keys"), setOfKeysType, keys.tree)
    val mixinPersisted = DefDef(NoMods, newTermName("mixinPersisted"), Nil, List(List(mkParam("value", TypeTree(T)), mkParam("idValue", Long))), TypeTree(), mixinPersistedBody)
    c.Expr[Entity[a]](mkAnon(List(AppliedTypeTree(Entity, List(TypeTree(T)))), List(entityCtor, keysField, mixinPersisted)))
  }

  def selectTerm( c : Context )( string : String ) = {
    import c.universe._
    string.split('.').view.map(s => newTermName(s)) match {
      case head +: tail => tail.foldLeft(Ident(head) : Tree)((a, b) => Select(a, b))
    }
  }

}
