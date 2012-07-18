package vorm.reflection

import reflect.mirror
import vorm._

sealed class Reflection 
  ( val t: mirror.Type,
    val javaClass: Class[_] )
  {
    lazy val fullName
      = mirrorQuirks.fullName(t.typeSymbol)

    lazy val name
      = mirrorQuirks.name(t.typeSymbol)

    lazy val properties
      : Map[String, Reflection]
      = mirrorQuirks.properties(t).view
          .map {
            s ⇒ mirrorQuirks.name(s) → 
                reflectionOf(s.typeSignature) 
          }
          .toMap

    lazy val generics
      : IndexedSeq[Reflection]
      = mirrorQuirks.generics(t).view
          .map(reflectionOf)
          .toIndexedSeq

    def inheritsFrom
      ( reflection : Reflection )
      : Boolean
      = reflection.fullName match {
          case n if n == "scala.Any" 
            ⇒ true
          case n if n == "scala.AnyVal" 
            ⇒ t.typeSymbol.isPrimitiveValueClass
          case _ 
            ⇒ reflection.javaClass.isAssignableFrom(javaClass) &&
              generics.view
                .zip(reflection.generics)
                .forall {case (a, b) => a.inheritsFrom(b)}
        }


    // lazy val methods
    //   = mirrorQuirks.methods(t).map { 
    //       s ⇒ 
    //         type MethodType = {
    //           def params: List[mirror.Symbol]
    //           def resultType: mirror.Type
    //         }

    //         val t = s.typeSignature.asInstanceOf[MethodType]
    //         val name = s.name.decoded.trim
    //         val arguments =
    //           t.params.map(
    //             p ⇒ Argument(mirrorQuirks.name(p), reflectionOf(p.typeSignature))
    //           )
    //         val result = reflectionOf(t.resultType)
    //         Method(name, arguments, result)
    //     }

    // lazy val methodNames
    // lazy val methodResults


  }
object Reflection {
  def apply
    ( implicit tag : TypeTag[_] )
    : Reflection
    = reflectionOf( tag )

  // sealed case class Argument(name: String, reflection: Reflection)
  // sealed case class Property(name: String, reflection: Reflection)
  // sealed case class Generic(index: Int, reflection: Reflection)
  // sealed case class Method(name: String, arguments: List[Argument], reflection: Reflection)
  // sealed case class Constructor(arguments: List[Argument])
}
