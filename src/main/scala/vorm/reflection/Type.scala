package vorm.reflection

import reflect.mirror
import vorm.reflection.Type._

/**
 * An abstraction over Scala's mirror functionality
 */
class Type(mt: mirror.Type) {

  lazy val fullName   =
    mt.typeSymbol.fullName
  lazy val name       =
    mt.typeSymbol.name.decoded
  lazy val generics   =
    mt.typeArguments
      .map(tpe(_))
  lazy val properties =
    mt.members.filter(m => !m.isMethod && m.owner == mt.typeSymbol)
      .map(s => Property(s.name.decoded.trim, tpe(s.typeSignature)))
  lazy val methods    = {
    def method(s: mirror.Symbol) = {
      type MethodType = {
        def params: List[mirror.Symbol]
        def resultType: mirror.Type
      }
      val t = s.typeSignature.asInstanceOf[MethodType]
      val name = s.name.decoded.trim
      val arguments =
        t.params.map(p => Argument(p.name.decoded, tpe(p.typeSignature)))
      val result = tpe(t.resultType)
      Method(name, arguments, result)
    }
    mt.members.filter(m => m.isMethod && m.owner == mt.typeSymbol)
      .map(method)
  }


  def methodResult(name: String, instance: AnyRef, args: List[Any] = Nil) =
    mirror.invoke(
      instance,
      mt.member(mirror.newTermName(name).asInstanceOf[mirror.Name])
    )(args: _*)

  def propertyValue(name: String, instance: AnyRef) =
    methodResult(_: String, _: AnyRef)


  /**
   * Scala bugs trickery and black magic
   */
  lazy val javaClass: Class[_]  =
    try mirror.typeToClass(mt)
    catch {
      case e: ClassNotFoundException =>
        def classByName(n: String) = mirror.symbolToClass(mirror.symbolForName(n))
        mt.typeSymbol.fullName match {
          case n if n == "scala.Any" => classOf[Any]
          case n => classByName(n)
        }
    }

  def inherits(t: Type): Boolean =
    t.fullName match {
      case n if n == "scala.Any" => true
      case n if n == "scala.AnyVal" =>
        mt.typeSymbol.isPrimitiveValueClass
      case _ =>
        t.javaClass.isAssignableFrom(javaClass) &&
        generics.zip(t.generics).forall {case (a, b) => a.inherits(b)}
    }


  def inherits[T: TypeTag]: Boolean =
    inherits(tpe[T])
//    mirror.typeToClass(tag[T].tpe)
//    mt <:< tag[T].tpe

//    mt <:< tag[T].tpe
//    if (generics.nonEmpty)
//      mt <:< tag[T].tpe
//    else
//      mt <:< mirror.classToType(tag[T].erasure)

  //  lazy val constructors =
  //    mt.members.filter(m => m.kind == "constructor" && m.owner == mt.typeSymbol)
  //      .map(method)
  //


  private lazy val methodByNameMap   = methods.map(m => m.name -> m).toMap
  private lazy val propertyByNameMap = properties.map(p => p.name -> p).toMap
  def method(name: String) = methodByNameMap(name)
  def property(name: String) = propertyByNameMap(name)

  override def toString = mt.toString
}

object Type {

  case class Property(
    name: String,
    t: Type
  ) {
    override def toString = name + ":" + t
  }

  case class Argument(
    name: String,
    t: Type
  ) {
    override def toString = name + ":" + t
  }

  case class Method(
    name: String,
    arguments: List[Argument],
    resultType: Type
  ) {
    override def toString = name + "(" + arguments.mkString(",") + "):" + resultType.toString
  }

}