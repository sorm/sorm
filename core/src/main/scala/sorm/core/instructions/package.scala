/**
 * Static instructions which are agnostic to the dynamic input values. 
 * 
 * Due to depending on none of the information generated at runtime, results of
 * post-processing these instructions can safely be cached. An example of such a
 * result is an SQL template or JDBC's PreparedStatement.
 */
package sorm.core.instructions

import sorm.core.subRef._


sealed trait Instruction
  [ Entity, Input, Output ]


/**
 * Selection Instructions
 */


sealed trait Select
  [ Entity, Input, Output ]
  extends Instruction[ Entity, Input, Output ]

/**
 * An edge-node that instructs to just proxy the input value.
 */
case class OutputValue
  [ Entity, Value ]
  extends Select[ Entity, Value, Value ]

/**
 * Output the whole entity without consuming any input.
 */
case class OutputEntity
  [ Entity ] 
  extends Select[ Entity, Unit, Entity ]

/**
 * Output just the specific sub-fields of the entity.
 */
case class OutputSubRefs
  [ Entity, SubRefsOutput ]
  ( subRefs : SubRefs[ Entity, SubRefsOutput ] )
  extends Select[ Entity, Unit, SubRefsOutput ]

case class Limit
  [ Entity, TailInput, Output ]
  ( tail : Select[ Entity, TailInput, Output ] )
  extends Select[ Entity, (Int, TailInput), Output ]

case class Offset
  [ Entity, TailInput, Output ]
  ( tail : Select[ Entity, TailInput, Output ] )
  extends Select[ Entity, (Int, TailInput), Output ]


/**
 * A sequence of instructions ordered by priority, as in SQL's 
 * `ORDER BY a, b DESC, c`.
 */
case class Order
  [ Entity, OrdersInput, TailInput, Output ]
  ( orders : Orders[ Entity, OrdersInput ],
    tail : Select[ Entity, TailInput, Output ] )
  extends Select[ Entity, (OrdersInput, TailInput), Output ]

sealed trait Orders[ Entity, Input ]

case class OrdersNil
  [ Entity ]
  extends Orders[ Entity, Unit ]

case class OrdersItem
  [ Entity, TailInput ]
  ( subRef : SubRef[ Entity, _ ], 
    reverse : Boolean, 
    tail : Orders[ Entity, TailInput ] )
  extends Orders[ Entity, (Boolean, TailInput) ]



sealed trait Filter
  [ Entity, Input, Output ]
  extends Select[ Entity, Input, Output ]

case class Equals
  [ Entity, ValueOutput, ValueInput, TailInput, Output ]
  ( subRef : SubRef[ Entity, ValueOutput ],
    value : Select[ Entity, ValueInput, ValueOutput ],
    tail : Select[ Entity, TailInput, Output ] )
  extends Filter[ Entity, (ValueInput, TailInput), Output ]


