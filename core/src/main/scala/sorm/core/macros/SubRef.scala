package sorm.core.macros

import language.experimental.macros
import reflect.runtime.universe._
import reflect.macros.Context

case class SubRef[ SourceT, TargetT ]( symbol : Symbol )

object SubRef {

  implicit def functionToSubRef
    [ SourceT, TargetT ]
    ( f : SourceT => TargetT ) 
    : SubRef[ SourceT, TargetT ]
    = macro functionToSubRefMacro[ SourceT, TargetT ]

  def functionToSubRefMacro
    [ SourceT : c.WeakTypeTag, TargetT : c.WeakTypeTag ]
    ( c : Context )
    ( f : c.Expr[ SourceT => TargetT ] )
    : c.Expr[ SubRef[ SourceT, TargetT ] ]
    = ???

}
