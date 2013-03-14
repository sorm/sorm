package sorm.core.instructions

// sealed trait Instruction[ Input, Output ]

sealed trait Select[ ReferenceEntity, Input, Output ]

case class Entity[ Output ] extends Select[ Output, Unit, Output ]

case class SelectSubRefs[ Entity, SubRefsResult ]
  ( subRefs : SubRefs[ Entity, SubRefsResult ] )
  extends Select[ Entity, Unit, SubRefsResult ]

case class Input[ A, B ]( value : B ) extends Select[ A, B, B ]


sealed trait Filter
  [ Entity, Input, Output ] 
  extends Select[ Entity, Input, Output ]

case class Equals
  [ Entity, ValueInput, ValueOutput, TailInput, TailOutput ]
  ( subRef : SubRef[ Entity, ValueOutput ], 
    value : Select[ Entity, ValueInput, ValueOutput ],
    tail : Select[ Entity, TailInput, TailOutput ] )
  extends Filter[ Entity, (ValueInput, TailInput), TailOutput ]


case class Limit[ A, B, C ]
  ( limit : Int, tail : Select[ A, B, C ] )
  extends Select[ A, (Int, B), C ]

case class Order[ A, B, C, D ]
  ( subRef : SubRef[ A, B ], reverse : Boolean, tail : Select[ A, C, D ] )
  extends Select[ A, (B, C), D ]


sealed trait SubRef[ Entity, Result ]

sealed trait SubRefs[ Entity, Result ]

case class SubRefs1[ A, B ]
  ( a : SubRef[ A, B ] ) 
  extends SubRefs[ A, B ]

case class SubRefs2[ A, B, C ]
  ( a : SubRef[ A, B ], b : SubRef[ A, C ] ) 
  extends SubRefs[ A, (B, C) ]

case class SubRefs3[ A, B, C, D ]
  ( a : SubRef[ A, B ], b : SubRef[ A, C ], c : SubRef[ A, D ] ) 
  extends SubRefs[ A, (B, C, D) ]
