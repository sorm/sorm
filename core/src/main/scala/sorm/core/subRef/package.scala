package sorm.core.subRef

import reflect.runtime.universe._

case class SubRef
  [ Entity, Value ]
  ( entitySymbol : Symbol,
    valueSymbol : Symbol )


sealed trait SubRefs
  [ Entity, Value ]

case class SubRefsValue
  [ A, B, C ]
  ( subRef : SubRef[ A, B ], 
    tail : SubRefs[ A, C ] )
  extends SubRefs[ A, (B, C) ]

case class SubRefsNil[ A ] extends SubRefs[ A, Unit ]
