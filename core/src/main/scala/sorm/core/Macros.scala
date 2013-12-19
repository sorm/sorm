package sorm.core

import reflect.macros.Context

private object Macros {

  def uniqueKey
    [ entity : c.WeakTypeTag, fields : c.WeakTypeTag ]
    ( c : Context )
    ( f : c.Expr[ entity => fields ] )
    : c.Expr[ UniqueKey[ entity, fields ] ]
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
    def mkSormRef(name: Name) = Select(selectByString(c)("sorm.core"), name)

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

  def selectByString( c : Context )( string : String ) = {
    import c.universe._
    string.split('.').view.map(s => newTermName(s)) match {
      case head +: tail => tail.foldLeft(Ident(head) : Tree)((a, b) => Select(a, b))
    }
  }

}
