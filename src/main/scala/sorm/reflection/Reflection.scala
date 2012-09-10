package sorm.reflection

import reflect.runtime.universe._
import reflect.runtime.{currentMirror => mirror}
import sext.Sext._
import ScalaApi._
import util.hashing.MurmurHash3
import collection.immutable.ListMap

class Reflection ( protected val t : Type ) {

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
    = t.toString

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
    : List[(String, Reflection)]
    = t.constructors.head.params.flatten
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
    = t.toInstanceOf[TypeRef].map(_.pre.s.decodedName)

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

}
object Reflection {
  def apply[ A : TypeTag ]  : Reflection = Reflection(typeOf[A])
  def apply( t : Type )     : Reflection = new Reflection(t)

}
