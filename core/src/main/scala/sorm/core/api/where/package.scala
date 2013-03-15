package sorm.core.api.where

import sorm.core.{instructions => Instructions}
import sorm.core.subRef._
import language.experimental.macros

/**
 * `.where( _.exists( _.genres, _.equals( _.name, "metal" ) ) )
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
class WhereComposer
  [ Driver, Entity, Input ]
  ( val instructions : Instructions.Filters[ Entity, Input ],
    val input : Input )
  extends DriverSpecificOperationsSupport[ Driver ]
  {

    def equals
      [ Value ]
      ( ref : Entity => Value,
        value : Value )
      : WhereComposer[ Driver, Entity, (Value, Input) ]
      = macro Macros.equals[ Driver, Entity, Value, Input ]

    def equalsImpl
      [ Value : DriverEqualsSupport ]
      ( ref : SubRef[ Entity, Value ],
        value : Value )
      : WhereComposer[ Driver, Entity, (Value, Input) ]
      = comparison( ref, Instructions.Equal, false, value )

    def notLargerImpl
      [ Value : DriverNotLargerSupport ]
      ( ref : SubRef[ Entity, Value ],
        value : Value )
      : WhereComposer[ Driver, Entity, (Value, Input) ]
      = comparison( ref, Instructions.Larger, true, value )
      
    def regexImpl
      [ Value : DriverRegexSupport ]
      ( ref : SubRef[ Entity, Value ],
        value : Value )
      : WhereComposer[ Driver, Entity, (Value, Input) ]
      = comparison( ref, Instructions.Regex, false, value )

    def existsImpl
      [ Value <: Traversable[ ValueItem ] : DriverExistsSupport,
        ValueItem,
        SubInput ]
      ( ref : SubRef[ Entity, Value ], 
        where : WhereComposer[ Driver, ValueItem, Unit ] => 
                WhereComposer[ Driver, ValueItem, SubInput ] )
      : WhereComposer[ Driver, Entity, (SubInput, Input) ]
      = ???

    /**
     * A helper
     */
    private def comparison
      [ Value ]
      ( ref : SubRef[ Entity, Value ],
        operator : Instructions.Operator,
        negative : Boolean,
        value : Value )
      : WhereComposer[ Driver, Entity, (Value, Input) ]
      = {
        val newInstructions = Instructions.Comparison(
          ref,
          Instructions.OutputValue[ Entity, Value ](),
          operator,
          negative,
          instructions
        )
        val newInput = (value, input)
        new WhereComposer( newInstructions, newInput )
      }
  }

private object Macros {

  import reflect.runtime.universe._
  import reflect.macros.Context

  def equals
    [ Driver,
      Entity : c.WeakTypeTag, 
      Value : c.WeakTypeTag,
      Input ]
    ( c : Context )
    ( ref : c.Expr[ Entity => Value ],
      value : c.Expr[ Value ] )
    : c.Expr[ WhereComposer[ Driver, Entity, (Value, Input) ] ]
    = ???

}

trait Exports {

}
