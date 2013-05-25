package sorm.core.api.where

import sorm.core.{instructions => Instructions}
import sorm.core.subRef._
import language.experimental.macros

/**
 * The stuff that drives these expressions:
 * `.where( _.exists( _.genres )( _.equals( _.name, "jazz" ) ) )`
 *
 * @tparam Driver The driver, for which the instruction is to be composed.
 * Needed for statically checking Driver's support for type-specific operations.
 * @tparam Entity The context entity. Required for `_.field` references.
 * In case of SQL this also defines the main table, to which others are to be 
 * joined.
 * @tparam Input The composed type of all input values for the generated 
 * instructions.
 * @param instructions Driver-agnostic instructions which are then to be 
 * compiled to driver-specific ones and cached.
 * @param input All the input values for generated instructions.
 */
class WhereComposition
  [ Driver, Entity, Input ]
  ( val instructions : Instructions.Filters[ Entity, Input ],
    val input : Input )
  extends DriverSpecificOperationsSupport[ Driver ]
  {

    def equals
      [ Value ]
      ( ref : Entity => Value,
        value : Value )
      : WhereComposition[ Driver, Entity, (Value, Input) ]
      = macro Macros.equals[ Driver, Entity, Value, Input ]

    def equals
      [ Value : DriverEqualsSupport ]
      ( ref : SubRef[ Entity, Value ],
        value : Value )
      : WhereComposition[ Driver, Entity, (Value, Input) ]
      = comparison( ref, Instructions.Equal, false, value )

    def notLarger
      [ Value : DriverNotLargerSupport ]
      ( ref : SubRef[ Entity, Value ],
        value : Value )
      : WhereComposition[ Driver, Entity, (Value, Input) ]
      = comparison( ref, Instructions.Larger, true, value )
      
    def regex
      [ Value : DriverRegexSupport ]
      ( ref : SubRef[ Entity, Value ],
        value : Value )
      : WhereComposition[ Driver, Entity, (Value, Input) ]
      = comparison( ref, Instructions.Regex, false, value )

    def exists
      [ Value[ ValueItem ] <: Traversable[ ValueItem ],
        ValueItem,
        SubInput ]
      ( ref : Entity => Value[ ValueItem ] )
      ( where : WhereComposition[ Driver, ValueItem, Unit ] =>
                WhereComposition[ Driver, ValueItem, SubInput ] )
      ( implicit support : DriverExistsSupport[ Value[ ValueItem ] ] )
      : WhereComposition[ Driver, Entity, (SubInput, Input) ]
      = ???

    def exists
      [ Value[ ValueItem ] <: Traversable[ ValueItem ],
        ValueItem,
        SubInput ]
      ( ref : SubRef[ Entity, Value[ ValueItem ] ] ) 
      ( where : WhereComposition[ Driver, ValueItem, Unit ] =>
                WhereComposition[ Driver, ValueItem, SubInput ] )
      ( implicit support : DriverExistsSupport[ Value[ ValueItem ] ] )
      : WhereComposition[ Driver, Entity, (SubInput, Input) ]
      = ???

    private def comparison
      [ Value ]
      ( ref : SubRef[ Entity, Value ],
        operator : Instructions.Operator,
        negative : Boolean,
        value : Value )
      : WhereComposition[ Driver, Entity, (Value, Input) ]
      = {
        val newInstructions = Instructions.Comparison(
          ref,
          Instructions.ReferenceValueInput[ Entity, Value ](),
          operator,
          negative,
          instructions
        )
        val newInput = (value, input)
        new WhereComposition( newInstructions, newInput )
      }
  }

private object Macros {

  import reflect.runtime.universe._
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
    [ Driver,
      Entity : c.WeakTypeTag, 
      Value : c.WeakTypeTag,
      Input ]
    ( c : Context )
    ( ref : c.Expr[ Entity => Value ],
      value : c.Expr[ Value ] )
    : c.Expr[ WhereComposition[ Driver, Entity, (Value, Input) ] ]
    = {

    import c.universe._

    val Entity = TypeTree(weakTypeOf[Entity])
    val Value = TypeTree(weakTypeOf[Value])
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

    val SubRef = Select(Select(Select(Ident(newTermName("sorm")), newTermName("core")), newTermName("subRef")), newTermName("SubRef"))
    val contextType = Select(reifyType(weakTypeOf[Entity]), newTermName("tpe"))
    val subFieldSymbols = Apply(Ident(newTermName("List")), reifiedSymbols)
    val subRef = Apply(TypeApply(SubRef, List(Entity, Value)), List(contextType, subFieldSymbols))

    c.Expr(Apply(Select(c.prefix.tree, newTermName("equals")), List(subRef, value.tree)))
  }

}

trait Exports {

}
