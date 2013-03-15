package sorm.core.instructions

import sorm.core.subRef._

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
