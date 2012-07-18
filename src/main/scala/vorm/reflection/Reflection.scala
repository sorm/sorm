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

  }
object Reflection {
  def apply
    ( implicit tag : TypeTag[_] )
    : Reflection
    = reflectionOf( tag )

}
