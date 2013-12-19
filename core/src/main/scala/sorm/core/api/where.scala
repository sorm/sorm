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

trait Exports {

}
