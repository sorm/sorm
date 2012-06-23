package vorm.reflection

import reflect.mirror
import vorm._

class Reflection (mt: mirror.Type, jc: Class[_]) {
  import Reflection._

  lazy val properties   = mirrorQuirks.properties(tag.tpe)
                            .map(s => Property(mirrorQuirks.name(s), reflection(s.typeSignature)))
  lazy val generics     = mirrorQuirks.generics(tag.tpe).view.zipWithIndex
                            .map{case(mt, i) => Generic(i, reflection(mt))}
                            .toList
  lazy val methods      = mirrorQuirks.methods(mt).map { s =>
                            type MethodType = {
                              def params: List[mirror.Symbol]
                              def resultType: mirror.Type
                            }

                            val t = s.typeSignature.asInstanceOf[MethodType]
                            val name = s.name.decoded.trim
                            val arguments =
                              t.params.map(p => Argument(mirrorQuirks.name(p), reflection(p.typeSignature)))
                            val result = reflection(t.resultType)
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
