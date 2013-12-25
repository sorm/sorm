package sorm.core.api

trait Persisted {
  val id: Long
}

/**
 * A type-class for mixing `Persisted` in.
 */
@annotation.implicitNotFound(msg = "PersistedMixiner instance for type ${a} is not declared")
trait PersistedMixiner[a] {
  def mixinPersisted(value: a, id: Long): a with Persisted
}
object PersistedMixiner {

  import language.experimental.macros

  def derive[a] = macro Macros.derive[a]

  object Macros {

    import scala.reflect.macros.Context

    /**
     * Generate an instance of `PersistedMixiner`.
     *
     * E.g., having the following class:
     * {{{
     * case class A( a : Int, b : String )
     * }}}
     * and triggering the macro conversion with the following:
     *
     * {{{
     * derivePersistedMixiner[ A ]
     * }}}
     *
     * should result in:
     *
     * {{{
     * implicit val someInstanceName = new PersistedMixiner[ A ]{
     *    def mixinPersisted( value : A, idValue : Long )
     *      = new A( value.a, value.b ) with Persisted {
     *          val id = idValue
     *          // a copy of A with `Persisted` mixed in
     *          override def copy
     *            ( a : Int = a,
     *              b : String = b )
     *            : A with Persisted
     *            = mixinPersisted( A(a, b), id )
     *          // product elements of `A` prepended with `id`
     *          override def productElement( n : Int ) : Any = n match {
     *            case 0 => id
     *            case 1 => a
     *            case 2 => b
     *            case _ => throw new IndexOutOfBoundsException( n.toString )
     *          }
     *          // product arity of `A` plus 1 (implying `id`)
     *          override def productArity = 3
     *          override def equals( that : Any ) = that match {
     *            case that : Persisted => id == that.id && super.equals( that )
     *            case _ => false
     *          }
     *        }
     * }
     * }}}
     */
    def derive[a: c.WeakTypeTag](c: Context) = {

      import c.universe._
      import Flag._

      val util = new sorm.core.util.Macros[c.type](c)

      val entityType = weakTypeOf[a]
      val fields = entityType.declarations.collect {case x: MethodSymbol if x.isParamAccessor => x}.toList

      def mkAnon(name: String, parents: List[Tree], methods: List[Tree]) = {
        val definition = ClassDef(Modifiers(FINAL), newTypeName(name), Nil, Template(parents, emptyValDef, methods))
        val application = Apply(Select(New(Ident(newTypeName(name))), nme.CONSTRUCTOR), List())
        Block(definition, application)
      }
      def mkSuperRef() = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)
      def mkCtor(superArgs: List[Tree]) = DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(List(Apply(mkSuperRef(), superArgs)), Literal(Constant(()))))
      def mkParam(name: String, tpe: Tree) = ValDef(Modifiers(PARAM), newTermName(name), tpe, EmptyTree)
      def mkDefaultParam(name: String, tpe: Tree, rhs: Tree) = ValDef(Modifiers(PARAM | DEFAULTPARAM), newTermName(name), tpe, rhs)

      val persistedType = util.selectType("sorm.core.api.Persisted")
      val withPersistedType = CompoundTypeTree(Template(List(TypeTree(entityType), persistedType), emptyValDef, Nil))

      val copyParams = fields.map(field => mkDefaultParam(field.name.toString, TypeTree(field.typeSignature), Select(Ident(newTermName("value")), field.name)))
      val copyCtor = Apply(Select(New(TypeTree(entityType)), nme.CONSTRUCTOR), fields.map(field => Ident(field.name)))
      val copyBody = Apply(Ident(newTermName("mixinPersisted")), List(copyCtor, Ident(newTermName("id"))))
      val copy = DefDef(Modifiers(OVERRIDE), newTermName("copy"), Nil, List(copyParams), withPersistedType, copyBody)

      val productElementZero = CaseDef(Literal(Constant(0)), EmptyTree, Ident(newTermName("id")))
      val productElementFields = fields.zipWithIndex.map {case (field, i) => CaseDef(Literal(Constant(i + 1)), EmptyTree, Ident(field.name))}
      val productElementFallback = CaseDef(Ident(nme.WILDCARD), EmptyTree, Throw(Apply(Select(New(util.selectType("IndexOutOfBoundsException")), nme.CONSTRUCTOR), List(Select(Ident(newTermName("n")), newTermName("toString"))))))
      val productElementBody = Match(Ident(newTermName("n")), productElementZero +: productElementFields :+ productElementFallback)
      val productElement = DefDef(Modifiers(OVERRIDE), newTermName("productElement"), Nil, List(List(mkParam("n", util.selectType("Int")))), util.selectType("Any"), productElementBody)

      val equalsId = Apply(Select(Ident(newTermName("id")), newTermName("$eq$eq")), List(Select(Ident(newTermName("that")), newTermName("id"))))
      val equalsSuper = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), newTermName("equals")), List(Ident(newTermName("that"))))
      val equalsIdAndSuper = Apply(Select(equalsId, newTermName("$amp$amp")), List(equalsSuper))
      val equalsPersisted = CaseDef(Bind(newTermName("that"), Typed(Ident(nme.WILDCARD), persistedType)), EmptyTree, equalsIdAndSuper)
      val equalsFallback = CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(false)))
      val equalsBody = Match(Ident(newTermName("that")), List(equalsPersisted, equalsFallback))
      val equals = DefDef(Modifiers(OVERRIDE), newTermName("equals"), Nil, List(List(mkParam("that", util.selectType("Any")))), TypeTree(), equalsBody)

      val mixinCtor = mkCtor(fields.map(field => Select(Ident(newTermName("value")), field.name)))
      val id = ValDef(NoMods, newTermName("id"), TypeTree(), Ident(newTermName("idValue")))
      val productArity = DefDef(Modifiers(OVERRIDE), newTermName("productArity"), Nil, Nil, TypeTree(), Literal(Constant(fields.length + 1)))
      val withPersistedInstance = {
        val name = entityType.typeSymbol.name.toString + "WithPersisted"
        mkAnon(name, List(TypeTree(entityType), persistedType), List(mixinCtor, id, copy, productElement, productArity, equals))
      }

      val entityCtor = mkCtor(Nil)
      val mixinPersisted = DefDef(NoMods, newTermName("mixinPersisted"), Nil, List(List(mkParam("value", TypeTree(entityType)), mkParam("idValue", util.selectType("Long")))), TypeTree(), withPersistedInstance)

      val persistedMixinerInstance = {
        val name = entityType.typeSymbol.name.toString + "PeristedMixiner"
        mkAnon(name, List(AppliedTypeTree(util.selectType("sorm.core.api.PersistedMixiner"), List(TypeTree(entityType)))), List(entityCtor, mixinPersisted))
      }
      c.Expr[PersistedMixiner[a]](persistedMixinerInstance)

    }
  }

}