package sorm.core.instructions

import sorm.core.subRef._

sealed trait Orders[ Entity ]

case class OrdersNil
  [ Entity ]
  extends Orders[ Entity ]

case class OrdersItem
  [ Entity ]
  ( subRef : SubRef[ Entity, _ ], 
    reverse : Boolean, 
    tail : Orders[ Entity ] )
  extends Orders[ Entity ]
