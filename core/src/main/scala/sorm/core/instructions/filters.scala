package sorm.core.instructions

import sorm.core.subRef._

sealed trait Filters
  [ Entity, Input ]

case class FiltersNil
  [ Entity ]
  extends Filters[ Entity, Unit ]

case class Or
  [ Entity, Input1, Input2, TailInput ]
  ( a : Filters[ Entity, Input1 ],
    b : Filters[ Entity, Input2 ],
    tail : Filters[ Entity, TailInput] )
  extends Filters[ Entity, (Input1, Input2, TailInput) ]

case class And
  [ Entity, Input1, Input2, TailInput ]
  ( a : Filters[ Entity, Input1 ],
    b : Filters[ Entity, Input2 ],
    tail : Filters[ Entity, TailInput] )
  extends Filters[ Entity, (Input1, Input2, TailInput) ]

case class Equals
  [ Entity, ValueOutput, ValueInput, TailInput ]
  ( subRef : SubRef[ Entity, ValueOutput ],
    value : Select[ Entity, ValueInput, ValueOutput ],
    tail : Filters[ Entity, TailInput ] )
  extends Filters[ Entity, (ValueInput, TailInput) ]
