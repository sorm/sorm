package sorm.core.api

import sorm.core.{instructions => Instructions}
import sorm.core._

/**
 * @tparam Driver The driver, for which the instruction is to be composed.
 * Needed for statically checking Driver's support for type-specific operations.
 * @tparam Entity The context entity. Required for `_.field` references.
 * In case of SQL this also defines the main table, to which others are to be
 * joined.
 * @tparam Input The composed type of all input values for the generated 
 * instructions.
 */
class SelectComposition
  [ Driver <: DriverSelectSupport, Entity, Input, Output ]
  ( val driver : Driver,
    val instructions : Instructions.Select[ Entity, Input, Output ],
    val input : Input )
  {

    def where
      [ FiltersInput ]
      ( filters : WhereComposition[ Driver, Entity, Unit ] =>
                  WhereComposition[ Driver, Entity, FiltersInput ] )
      : SelectComposition[ Driver, Entity, (FiltersInput, Input), Output ]
      = ???

    /**
     *
     * @param ref A reference to the field, by which to order.
     * @param reverse Whether the ordering should be descending.
     * @param support An implicit evidence of a driver support for ordering
     *                by values of presented Value type.
     * @tparam Value Type of referred value.
     * @return New SelectComposition.
     */
    def order
      [ Value ]
      ( ref : FieldRef[ Entity, Value ],
        reverse : Boolean )
      ( implicit support : OrderSupport[ Driver, Value ] )
      : SelectComposition[ Driver, Entity, Input, Output ]
      = new SelectComposition(
          driver,
          Instructions.Select.Order(
            ref,
            reverse,
            instructions
          ),
          input
        )

  }

/**
 * Implicit instances of this type denote the driver's support for ordering by
 * specific type.
 */
trait OrderSupport[ Driver, -Value ]

trait InstanceSelectSupport[ Driver <: DriverSelectSupport ] {

  val driver : Driver

  def select
    [ Entity ]
    : SelectComposition[ Driver, Entity, Unit, Seq[ Entity ] ]
    = new SelectComposition( driver, Instructions.Select.OutputEntity[ Entity ](), Unit )

}

trait DriverSelectSupport {

}

