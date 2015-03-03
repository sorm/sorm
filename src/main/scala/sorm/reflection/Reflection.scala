package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sext._, embrace._
import ScalaApi._
import util.hashing.MurmurHash3
import collection.immutable.ListMap

class Reflection ( protected val ambiguousType : Type ) {
  
  protected val t =
    ambiguousType match {
      case t : NullaryMethodType => t.resultType
      case t => t
    }  
  
  protected def s : Symbol = t.s

  override def toString = t.toString

  override def hashCode
    = MurmurHash3.finalizeHash(t.typeSymbol.hashCode, generics.hashCode)

  override def equals ( other : Any )
    = other match {
        case other : Reflection =>
          t =:= other.t
        case _ =>
          false
      }

  def <:< ( other : Reflection ) = t <:< other.t
  def =:= ( other : Reflection ) = t =:= other.t

  def properties
    = t.properties
        .map{ s => s.decodedName -> Reflection(s.t) }
        .toMap
  def generics
    = t match {
        case t : TypeRef => t.args.view.map(Reflection(_)).toIndexedSeq
        case _ => Vector()
      }
  def name
    = s.decodedName
  def fullName
    = s.ancestors.foldRight(""){ (s, text) =>
        if( text == "" ) s.decodedName
        else if( s.owner.isClass ) text + "#" + s.decodedName
        else text + "." + s.decodedName
      }
  def signature : String
    = t.toString

  def instantiate
    ( params : Seq[Any] )
    : Any
    = s.instantiate(t.constructors.head, params)

  def propertyValue
    ( name : String,
      instance : AnyRef )
    : Any
    = instance.getClass.getMethods.find(_.getName == name).get.invoke(instance)

  def propertyValues
    ( instance : AnyRef )
    : Map[String, Any]
    = properties.keys.view.zipBy{ propertyValue(_, instance) }.toMap

  def primaryConstructorArguments
    : List[(String, Reflection)]
    = t.constructors.head.paramss.flatten
        .map{ s => s.decodedName -> Reflection(s.t) }

  /**
   * Either the type itself if it's not mixed in or the first of its parents
   */
  def mixinBasis
    = t match {
        case t : RefinedType => Reflection(t.parents.head)
        case _ => this
      }

  def containerObjectName : Option[String]
    = t.trying(_.asInstanceOf[TypeRef]).map(_.pre.s.decodedName)

  def containerObject : Option[Any]
    = t match {
        case t : TypeRef =>
          t.pre.typeSymbol match {
            case s =>
              Some(
                mirror.reflectModule(
                  s.owner.typeSignature.member(s.name.toTermName).asModule
                ).instance
              )
          }
        case _ => None
      }

  def isCaseClass
    = s match {
        case s : ClassSymbol => s.isCaseClass
        case _ => false
      }

  def javaClass = t.javaClass
}
object Reflection {
  def apply[ A : TypeTag ]  : Reflection = Reflection(typeOf[A])
  def apply( t : Type )     : Reflection = new Reflection(t)

}
