package sorm.core.api.where

import sorm.core.{instructions => Instructions}
import sorm.core.subRef._

/**
 * `.where( _.exists( _.genres, _.equals( _.name, "metal" ) ) )
 */
class WhereComposer
  [ Context, Input ]
  ( val instructions : Instructions.Filters[ Context, Input ],
    val input : Input )
  {

    def equalsImpl
      [ Value ]
      ( ref : SubRef[ Context, Value ],
        value : Value )
      : WhereComposer[ Context, (Value, Input) ]
      = {
        val newInstructions = Instructions.Comparison(
          ref,
          Instructions.OutputValue[ Context, Value ](),
          Instructions.Equal,
          false,
          instructions
        )
        val newInput = (value, input)
        new WhereComposer( newInstructions, newInput )
      }

    def existsImpl
      [ Value <: Traversable[ ValueItem ],
        ValueItem,
        SubInput ]
      ( ref : SubRef[ Context, Value ], 
        where : WhereComposer[ ValueItem, Unit ] => 
                WhereComposer[ ValueItem, SubInput ] )
      : WhereComposer[ Context, (SubInput, Input) ]
      = ???

  }

private object Macros {

}

trait Exports {


}
