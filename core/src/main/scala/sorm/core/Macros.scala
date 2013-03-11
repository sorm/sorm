package sorm.core

import language.experimental.macros
import reflect.runtime.universe._
import reflect.macros.Context

private object Macros {

  def entity
    [ A : c.WeakTypeTag ]
    ( c : Context )
    ( indexed : c.Expr[ Set[ Key[ A ] ] ], 
      unique : c.Expr[ Set[ Key[ A ] ] ] )
    : c.Expr[ Entity[ A ] ]
    = {
      import c.universe._
      ???
    }

  def key1
    [ SiteT : c.WeakTypeTag, 
      FieldT : c.WeakTypeTag ]
    ( c : Context )
    ( f : c.Expr[ SiteT => FieldT ] )
    : c.Expr[ Key[ SiteT ] ]
    = ??? 

  def key2
    [ SiteT : c.WeakTypeTag, 
      FieldT1 : c.WeakTypeTag,
      FieldT2 : c.WeakTypeTag ]
    ( c : Context )
    ( f1 : c.Expr[ SiteT => FieldT1 ],
      f2 : c.Expr[ SiteT => FieldT2 ] )
    : c.Expr[ Key[ SiteT ] ]
    = ??? 

  def key3
    [ SiteT : c.WeakTypeTag, 
      FieldT1 : c.WeakTypeTag,
      FieldT2 : c.WeakTypeTag,
      FieldT3 : c.WeakTypeTag ]
    ( c : Context )
    ( f1 : c.Expr[ SiteT => FieldT1 ],
      f2 : c.Expr[ SiteT => FieldT2 ],
      f3 : c.Expr[ SiteT => FieldT3 ] )
    : c.Expr[ Key[ SiteT ] ]
    = ??? 

  def key4
    [ SiteT : c.WeakTypeTag, 
      FieldT1 : c.WeakTypeTag,
      FieldT2 : c.WeakTypeTag,
      FieldT3 : c.WeakTypeTag,
      FieldT4 : c.WeakTypeTag ]
    ( c : Context )
    ( f1 : c.Expr[ SiteT => FieldT1 ],
      f2 : c.Expr[ SiteT => FieldT2 ],
      f3 : c.Expr[ SiteT => FieldT3 ],
      f4 : c.Expr[ SiteT => FieldT4 ] )
    : c.Expr[ Key[ SiteT ] ]
    = ??? 


}
