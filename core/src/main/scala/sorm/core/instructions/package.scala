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
  extends Select[ Entity, Unit, Seq[ Entity ] ]

/**
 * Output just the specific sub-fields of the entity.
 */
case class OutputSubRefs
  [ Entity, SubRefsOutput ]
  ( subRefs : SubRefs[ Entity, SubRefsOutput ] )
  extends Select[ Entity, Unit, Seq[ SubRefsOutput ] ]

case class Limit
  [ Entity, TailInput, Output ]
  ( tail : Select[ Entity, TailInput, Output ] )
  extends Select[ Entity, (Int, TailInput), Output ]

case class Offset
  [ Entity, TailInput, Output ]
  ( tail : Select[ Entity, TailInput, Output ] )
  extends Select[ Entity, (Int, TailInput), Output ]

case class Order
  [ Entity, TailInput, Output ]
  ( subRef : SubRef[ Entity, _ ],
    reverse : Boolean,
    tail : Select[ Entity, TailInput, Output ] )
  extends Select[ Entity, TailInput, Output ]

case class Filter
  [ Entity, FiltersInput, TailInput, Output ]
  ( filters : Filters[ Entity, FiltersInput ] )
  extends Select[ Entity, (FiltersInput, TailInput), Output ]


case class Insert
  [ Entity, Input ]
  ( input : SubRefs[ Entity, Input ] )
  extends Instruction[ Entity, Input, Unit ]
  
case class Update
  [ Entity, Input, FiltersInput ]
  ( input : SubRefs[ Entity, Input ],
    filters : Filters[ Entity, FiltersInput ] )
  extends Instruction[ Entity, (Input, FiltersInput), Unit ]
  
case class Delete
  [ Entity, FiltersInput ]
  ( filters : Filters[ Entity, FiltersInput ] )
  extends Instruction[ Entity, FiltersInput, Unit ]

