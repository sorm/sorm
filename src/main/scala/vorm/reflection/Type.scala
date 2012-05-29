package vorm.reflection

import reflect.mirror
import vorm.mirrorQuirks
import util.MurmurHash3


/**
 * An abstraction over Scala's mirror functionality
 */
class Type(protected val mt: mirror.Type, jc: Option[Class[_]]) {



  lazy val mixinBasis =
    tpe(mirrorQuirks.mixinBasis(mt))
  lazy val signature: String =
    generics match {
      case Nil => fullName
      case _ => fullName + "[" + generics.map(_.signature).mkString(", ") + "]"
    }
  lazy val fullName     =
    mirrorQuirks.fullName(mt.typeSymbol)
  lazy val name         =
    mirrorQuirks.name(mt.typeSymbol)
  lazy val generics     =
    mirrorQuirks.generics(mt)
      .map(tpe(_))
  lazy val properties   =
    mirrorQuirks.properties(mt).toList
      .map(s => PropertyProperties(mirrorQuirks.name(s), tpe(s.typeSignature)))

  lazy val methods      =
    mirrorQuirks.methods(mt).toList.map { s =>
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
  lazy val constructors =
    mirrorQuirks.constructors(mt)
      .map(
        _.typeSignature
          .asInstanceOf[{def params: List[mirror.Symbol]}].params
          .map(p => ArgumentProperties(p.name.decoded, tpe(p.typeSignature)))
      )
      .map(ConstructorProperties)

  lazy val javaClass =
    jc getOrElse mirrorQuirks.javaClass(mt)

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

  def propertyValue(name: String, instance: AnyRef) =
    methodResult(name, instance)

  def instance(params: Map[String, Any]) = {
    if (mirrorQuirks.isInner(mt.typeSymbol))
      throw new UnsupportedOperationException("Dynamic instantiation of inner classes is not supported")

    val args =
      constructors.head
        .arguments.map(_.name)
        .map(params)

    val constructor = javaClass.getConstructors.head

    constructor.newInstance(args.asInstanceOf[List[Object]]: _*)
  }

  override def toString = mt.toString
  override def equals(x: Any) =
    x match {
      case x: Type =>
        eq(x) ||
        mt.typeSymbol == x.mt.typeSymbol &&
        generics == x.generics
      case _ => super.equals(x)
    }
  override def hashCode =
    MurmurHash3.finalizeHash(mt.typeSymbol.hashCode, generics.hashCode)
}