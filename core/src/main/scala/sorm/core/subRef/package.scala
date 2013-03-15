package sorm.core.subRef

sealed trait SubRef[ Entity, Result ]

sealed trait SubRefs[ Entity, Result ]

case class SubRefsValue[ A, B, C ]
  ( subRef : SubRef[ A, B ], tail : SubRefs[ A, C ] )
  extends SubRefs[ A, (B, C) ]

case class SubRefsNil[ A ] extends SubRefs[ A, Unit ]
