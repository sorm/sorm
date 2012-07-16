package vorm.reflection

import reflect.mirror
import vorm._

sealed class Reflection (mt: mirror.Type, jc: Class[_]) {
  import Reflection._

  lazy val properties
    = mirrorQuirks.properties(mt).view
        .map {
          s ⇒ Property(mirrorQuirks.name(s), reflectionOf(s.typeSignature)) 
        }
        .toMap

  lazy val generics
    = mirrorQuirks.generics(mt).view.zipWithIndex
        .map {
          case (t, i) ⇒ Generic(i, reflectionOf(t))
        }
        .toIndexedSeq

  lazy val methods
    = mirrorQuirks.methods(mt).map { 
        s ⇒ 
          type MethodType = {
            def params: List[mirror.Symbol]
            def resultType: mirror.Type
          }

          val t = s.typeSignature.asInstanceOf[MethodType]
          val name = s.name.decoded.trim
          val arguments =
            t.params.map(
              p ⇒ Argument(mirrorQuirks.name(p), reflectionOf(p.typeSignature))
            )
          val result = reflectionOf(t.resultType)
          Method(name, arguments, result)
      }

}
object Reflection {
  sealed case class Argument(name: String, reflection: Reflection)
  sealed case class Property(name: String, reflection: Reflection)
  sealed case class Generic(index: Int, reflection: Reflection)
  sealed case class Method(name: String, arguments: List[Argument], reflection: Reflection)
  sealed case class Constructor(arguments: List[Argument])
}
