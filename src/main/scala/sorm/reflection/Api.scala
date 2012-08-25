package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sorm.extensions.Extensions._

trait Api {

  def t : Type

  private val api = new ScalaApi.TypeApi(t)

  def properties
     = api.members.filter{_.isTerm}.filter{!_.isMethod}
         .map{ s => api.name(s) -> Reflection(s.typeSignature) }
         .toMap
  def generics
     = t match {
         case t : TypeRef => t.args.toStream.map{ Reflection(_) }
       }
  def signature : String
     = if( generics.isEmpty ) api.fullName
       else api.fullName + "[" + generics.map(_.signature).mkString(", ") + "]"
  override def toString = t.toString


}
