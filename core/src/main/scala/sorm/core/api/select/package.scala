package sorm.core.api.select

import sorm.core.{instructions => Instructions}
import sorm.core.api.{where => Where}
import sorm.core.subRef._

/**
 * @tparam Driver The driver, for which the instruction is to be composed.
 * Needed for statically checking Driver's support for type-specific operations.
 * @tparam Entity The context entity. Required for `_.field` references.
 * In case of SQL this also defines the main table, to which others are to be 
 * joined.
 * @tparam Input The composed type of all input values for the generated 
 * instructions.
 */
class SelectComposer
  [ Driver, Entity, Input, Output ]
  ( val instructions : Instructions.Select[ Entity, Input, Output ],
    val input : Input )
  {

    def where
      [ FiltersInput ]
      ( filters : Where.WhereComposer[ Driver, Entity, Unit ] =>
                  Where.WhereComposer[ Driver, Entity, FiltersInput ] )
      : SelectComposer[ Driver, Entity, (FiltersInput, Input), Output ]
      = ???

    def order
      [ Value ]
      ( ref : SubRef[ Entity, Value ],
        reverse : Boolean )
      : SelectComposer[ Driver, Entity, Input, Output ]
      = new SelectComposer(
          Instructions.Order(
            ref,
            reverse,
            instructions
          ),
          input
        )

  }

