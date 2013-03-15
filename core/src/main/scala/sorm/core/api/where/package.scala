package sorm.core.api.where

import sorm.core.{instructions => Instructions}
import sorm.core.subRef._
import language.experimental.macros

/**
 * `.where( _.exists( _.genres, _.equals( _.name, "metal" ) ) )
 */
class WhereComposer
  [ Entity, Input ]
  ( val instructions : Instructions.Filters[ Entity, Input ],
    val input : Input )
  {

    def equals
      [ Value ]
      ( ref : Entity => Value,
        value : Value )
      : WhereComposer[ Entity, (Value, Input) ]
      = macro Macros.equals[ Entity, Value, Input ]

    def equalsImpl
      [ Value ]
      ( ref : SubRef[ Entity, Value ],
        value : Value )
      : WhereComposer[ Entity, (Value, Input) ]
      = {
        val newInstructions = Instructions.Comparison(
          ref,
          Instructions.OutputValue[ Entity, Value ](),
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
      ( ref : SubRef[ Entity, Value ], 
        where : WhereComposer[ ValueItem, Unit ] => 
                WhereComposer[ ValueItem, SubInput ] )
      : WhereComposer[ Entity, (SubInput, Input) ]
      = ???

  }

private object Macros {

  import reflect.runtime.universe._
  import reflect.macros.Context

  def equals
    [ Entity : c.WeakTypeTag, 
      Value : c.WeakTypeTag,
      Input ]
    ( c : Context )
    ( ref : c.Expr[ Entity => Value ],
      value : c.Expr[ Value ] )
    : c.Expr[ WhereComposer[ Entity, (Value, Input) ] ]
    = ???

}

trait Exports {


}
