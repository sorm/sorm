package sorm.reflection

import reflect.runtime.universe._

class Reflection
  ( val t : Type )
  extends Api
  {

  }
object Reflection {

  def apply[ A : TypeTag ] : Reflection = Reflection(typeOf[A])
  def apply( t : Type ) : Reflection = new Reflection(t)

//   val cache
//    = new collection.mutable.HashMap[(Type, Class[_]), Reflection] {
//        override def default
//          ( key : (Type, Class[_]) )
//          = {
//            val value = new Reflection(key._1, key._2)
//            update(key, value)
//            value
//          }
//      }
//
//  def apply
//    [ T ]
//    ( implicit tag : TypeTag[T] )
//    : Reflection
//    = cache( tag.tpe -> tag.erasure )

  // def apply
  //   ( mt : Type )
  //   : Reflection 
  //   = cache( mt, MirrorQuirks.javaClass(mt) )

}
