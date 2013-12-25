package sorm.core.api

import sorm.core.{instructions => Instructions}
import sorm.core._
import language.experimental.macros

/**
 * The stuff that drives these expressions:
 * `.where( _.exists( _.genres )( _.equals( _.name, "jazz" ) ) )`
 *
 * @tparam driver The driver, for which the instruction is to be composed.
 * Needed for statically checking Driver's support for type-specific operations.
 * @tparam entity The context entity. Required for `_.field` references.
 * In case of SQL this also defines the main table, to which others are to be 
 * joined.
 * @tparam input The composed type of all input values for the generated 
 * instructions.
 * @param instructions Driver-agnostic instructions which are then to be 
 * compiled to driver-specific ones and cached.
 * @param input All the input values for generated instructions.
 */
class WhereComposition
  [ driver, entity, input ]
  ( val instructions : Instructions.Filters[ entity, input ],
    val input : input )
  extends DriverSpecificOperationsSupport[ driver ]
  {
  import WhereComposition._

  def equals
    [ value ]
    ( ref : entity => value,
      value : value )
    : WhereComposition[ driver, entity, (value, input) ]
    = macro Macros.equals[ driver, entity, value, input ]

  def equals
    [ value : DriverEqualsSupport ]
    ( ref : FieldRef[ entity, value ],
      value : value )
    : WhereComposition[ driver, entity, (value, input) ]
    = comparison( ref, Instructions.Operator.Equal, false, value )

  def notLarger
    [ value : DriverNotLargerSupport ]
    ( ref : FieldRef[ entity, value ],
      value : value )
    : WhereComposition[ driver, entity, (value, input) ]
    = comparison( ref, Instructions.Operator.Larger, true, value )

  def regex
    [ value : DriverRegexSupport ]
    ( ref : FieldRef[ entity, value ],
      value : value )
    : WhereComposition[ driver, entity, (value, input) ]
    = comparison( ref, Instructions.Operator.Regex, false, value )

  def exists
    [ value[ a ] <: Traversable[ a ],
      valueItem,
      subInput ]
    ( ref : entity => value[ valueItem ] )
    ( where : WhereComposition[ driver, valueItem, Unit ] =>
              WhereComposition[ driver, valueItem, subInput ] )
    ( implicit support : DriverExistsSupport[ value[ valueItem ] ] )
    : WhereComposition[ driver, entity, (subInput, input) ]
    = ???

  def exists
    [ value[ a ] <: Traversable[ a ],
      valueItem,
      subInput ]
    ( ref : FieldRef[ entity, value[ valueItem ] ] )
    ( where : WhereComposition[ driver, valueItem, Unit ] =>
              WhereComposition[ driver, valueItem, subInput ] )
    ( implicit support : DriverExistsSupport[ value[ valueItem ] ] )
    : WhereComposition[ driver, entity, (subInput, input) ]
    = ???

  private def comparison
    [ value ]
    ( ref : FieldRef[ entity, value ],
      operator : Instructions.Operator,
      negative : Boolean,
      value : value )
    : WhereComposition[ driver, entity, (value, input) ]
    = {
      val newInstructions = Instructions.Comparison(
        ref,
        Instructions.ReferenceValueInput[ entity, value ](),
        operator,
        negative,
        instructions
      )
      val newInput = (value, input)
      new WhereComposition( newInstructions, newInput )
    }



}
object WhereComposition {
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
      val util = new sorm.core.util.Macros[c.type](c)

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

      val SubRef = util.selectTerm("sorm.core.FieldRef")
      val contextType = Select(reifyType(weakTypeOf[entity]), newTermName("tpe"))
      val subFieldSymbols = Apply(Ident(newTermName("List")), reifiedSymbols)
      val subRef = Apply(TypeApply(SubRef, List(Entity, Value)), List(contextType, subFieldSymbols))

      c.Expr(Apply(Select(c.prefix.tree, newTermName("equals")), List(subRef, value.tree)))
    }

  }
}
