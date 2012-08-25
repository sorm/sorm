package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sorm.extensions.Extensions._
import ScalaApi._

class Reflection ( protected val t : Type ) {

  protected def s : Symbol = t.s

  override def toString = t.toString

  override def hashCode = t.hashCode
  override def equals ( other : Any )
    = other match {
        case other : Reflection =>
          t =:= other.t
        case _ =>
          false
      }

  def inheritsFrom ( other : Reflection ) = t <:< other.t

  def properties
    = t.properties
        .map{ s => s.decodedName -> Reflection(s.t) }
        .toMap
  def generics
    = t match {
        case t : TypeRef => t.args.view.map{ Reflection(_) }.toIndexedSeq
      }
  def name
    = s.decodedName
  def fullName
    = s.ancestors.foldRight(""){ (s, text) =>
        if( text == "" ) s.decodedName
        else if( s.owner.kind == "class" ) text + "#" + s.decodedName
        else text + "." + s.decodedName
      }
  def signature : String
    = if( generics.isEmpty ) fullName
      else fullName + "[" + generics.map(_.signature).mkString(", ") + "]"

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

  def primaryConstructorArguments
    : Map[String, Reflection]
    = t.constructors.head.params.flatten
        .map{ s => s.decodedName -> Reflection(s.t) }
        .toMap

  /**
   * Either the type itself if it's not mixed in or the first of its parents
   */
  def mixinBasis
    = t match {
        case t : RefinedType => Reflection(t.parents.head)
        case _ => this
      }

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
