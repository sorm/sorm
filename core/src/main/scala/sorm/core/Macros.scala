package sorm.core

import language.experimental.macros
import reflect.runtime.universe._
import reflect.macros.Context

private object Macros {

  def entity
    [ A : c.WeakTypeTag ]
    ( c : Context )
    ( indexed : c.Expr[ Set[ Key[ A, _ ] ] ], 
      unique : c.Expr[ Set[ Key[ A, _ ] ] ] )
    : c.Expr[ Entity[ A ] ]
    = {
      import c.universe._
      ???
    }

  def key
    [ SiteT : c.WeakTypeTag, FieldsT : c.WeakTypeTag ]
    ( c : Context )
    ( f : c.Expr[ SiteT => FieldsT ] )
    : c.Expr[ Key[ SiteT, FieldsT ] ]
    = ??? 

}
