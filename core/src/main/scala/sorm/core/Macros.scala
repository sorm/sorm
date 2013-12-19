package sorm.core

import language.experimental.macros
import reflect.runtime.universe._
import reflect.macros.Context

private object Macros {

  def entity
    [ a : c.WeakTypeTag ]
    ( c : Context )
    ( keys : c.Expr[ Set[ Key[ a, _ ] ] ] )
    : c.Expr[ Entity[ a ] ]
    = {
      import c.universe._
      ???
    }

  def uniqueKey
    [ entity : c.WeakTypeTag, fields : c.WeakTypeTag ]
    ( c : Context )
    ( f : c.Expr[ entity => fields ] )
    : c.Expr[ UniqueKey[ entity, fields ] ]
    = ??? 

}
