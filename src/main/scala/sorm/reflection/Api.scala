package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sorm.extensions.Extensions._
import ScalaApi._

trait Api {

  def t : Type

  def s : Symbol = t.s

  override def toString = t.toString

  def properties
    = t.properties
        .map{ s => s.decodedName -> Reflection(s.t) }
        .toMap
  def generics
    = t match {
        case t : TypeRef => t.args.toStream.map{ Reflection(_) }
      }
  def signature : String
    = if( generics.isEmpty ) s.fullName
      else s.fullName + "[" + generics.map(_.signature).mkString(", ") + "]"
  
  def inheritsFrom ( other : Api )
    = t <:< other.t

  def instantiate
    ( params : Map[String, Any] )
    : Any
    = t.constructors
        .view
        .zipBy{ _.params.view.flatten.map{_.decodedName} }
        .find{ _._2.toSet == params.keySet }
        .map{ case (c, ps) => s.instantiate( c, ps.map{params} ) }
        .get

  def instantiate
    ( params : Seq[Any] )
    : Any
    = s.instantiate(t.constructors.head, params)


}
