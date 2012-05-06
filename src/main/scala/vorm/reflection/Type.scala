package vorm.reflection

import reflect.mirror
import vorm.mirrorQuirks

/**
 * An abstraction over Scala's mirror functionality
 */
class Type(mt: mirror.Type) {

  lazy val signature: String =
    generics match {
      case Nil => fullName
      case _ => fullName + "[" + generics.map(_.signature).mkString(", ") + "]"
    }
  lazy val fullName     =
    mirrorQuirks.fullName(mt.typeSymbol)
  lazy val name         =
    mt.typeSymbol.name.decoded
  lazy val generics     =
    mt.typeArguments.toList
      .map(tpe(_))
  lazy val properties   =
    mt.members.toList.filter(m => !m.isMethod && m.owner == mt.typeSymbol)
      .map(s => PropertyProperties(s.name.decoded.trim, tpe(s.typeSignature)))

  lazy val methods      = {
    def methodProperties(s: mirror.Symbol) = {
      type MethodType = {
        def params: List[mirror.Symbol]
        def resultType: mirror.Type
      }
      val t = s.typeSignature.asInstanceOf[MethodType]
      val name = s.name.decoded.trim
      val arguments =
        t.params.map(p => ArgumentProperties(p.name.decoded, tpe(p.typeSignature)))
      val result = tpe(t.resultType)
      MethodProperties(name, arguments, result)
    }

    mt.members.toList.filter(m => m.isMethod && m.owner == mt.typeSymbol)
      .map(methodProperties)
  }
  lazy val constructors =
    mt.members.toList.filter(m => m.kind == "constructor" && m.owner == mt.typeSymbol)
      .map(
        _.typeSignature.asInstanceOf[{def params: List[mirror.Symbol]}].params
          .map(p => ArgumentProperties(p.name.decoded, tpe(p.typeSignature))))
      .map(ConstructorProperties)
      .reverse

  lazy val javaClass: Class[_] =
    try mirror.typeToClass(mt)
    catch {
      case e: ClassNotFoundException =>
        def classByName(n: String) = mirror.symbolToClass(mirror.symbolForName(n))

        def javaClassName(s: mirror.Symbol): String =
          s.owner match {
            case o if o.isPackageClass =>
              s.fullName
            case o if o.isClass =>
              javaClassName(o) + "$" + s.name.decoded.trim
          }

        mt.typeSymbol match {
          case s if s.fullName == "scala.Any" => classOf[Any]
          case s => classByName(javaClassName(s))
        }
    }

  /**
   * Scala bugs trickery and black magic.
   * Should have been based on <code>mt <:< tag[T].tpe</code> but there are bugs in there.
   */
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

  lazy val methodByNameMap   = methods.map(m => m.name -> m).toMap
  lazy val propertyByNameMap = properties.map(p => p.name -> p).toMap
  def method(name: String)   = methodByNameMap(name)
  def property(name: String) = propertyByNameMap(name)

  def methodResult(name: String, instance: AnyRef, args: List[Any] = Nil) =
    javaClass.getMethods.find(_.getName == name).get.invoke(instance)

//    mirror.invoke(
//      instance,
//      mt.member(mirror.newTermName(name).asInstanceOf[mirror.Name])
//    )(args: _*)

  def propertyValue(name: String, instance: AnyRef) =
    methodResult(name, instance)


  override def toString = mt.toString
}