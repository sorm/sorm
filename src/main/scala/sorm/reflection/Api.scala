package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sorm.extensions.Extensions._
import ScalaApi._

trait Api {

  protected def t : Type

  protected def s : Symbol = t.s

  override def toString = t.toString

  def properties
    = t.properties
        .map{ s => s.decodedName -> Reflection(s.t) }
        .toMap
  def generics
    = t match {
        case t : TypeRef => t.args.view.map{ Reflection(_) }.toIndexedSeq
      }
  def fullName
    = s.ancestors.foldRight(""){ (s, text) =>
        if( text == "" ) s.decodedName
        else if( s.owner.kind == "class" ) text + "#" + s.decodedName
        else text + "." + s.decodedName
      }
  def signature : String
    = if( generics.isEmpty ) fullName
      else fullName + "[" + generics.map(_.signature).mkString(", ") + "]"
  
  def inheritsFrom ( other : Api ) = t <:< other.t
  def <:< ( other : Api ) = t <:< other.t
  def !<:< ( other : Api ) = !(t <:< other.t)
  def =:= ( other : Api ) = t =:= other.t
  def !=:= ( other : Api ) = !(t =:= other.t)

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


  private lazy val javaMethodsByName
    = t.javaClass.getMethods.groupBy{_.getName}

  def propertyValue
    ( name : String,
      instance : AnyRef )
    : Any
    = javaMethodsByName(name).head.invoke( instance )

  def propertyValues
    ( instance : AnyRef )
    : Map[String, Any]
    = properties.keys.view.zipBy{ propertyValue(_, instance) }.toMap



}
