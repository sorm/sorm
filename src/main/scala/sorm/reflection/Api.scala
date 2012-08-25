package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sorm.extensions.Extensions._
import ScalaApi._
trait Api {

  def t : Type

  def properties
     = t.properties
         .map{ s => s.decodedName -> Reflection(s.typeSignature) }
         .toMap
  def generics
     = t match {
         case t : TypeRef => t.args.toStream.map{ Reflection(_) }
       }
  def signature : String
     = if( generics.isEmpty ) t.fullName
       else t.fullName + "[" + generics.map(_.signature).mkString(", ") + "]"

  override def toString = t.toString


}
