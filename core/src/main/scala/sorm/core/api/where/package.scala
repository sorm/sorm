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
    = ???

}

trait Exports {

}
