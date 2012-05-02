package vorm.reflection

import reflect.mirror

class MethodReflection(
  val name: String,
  val owner: TypeReflection,
  val argumentTypes: List[TypeReflection],
  val resultType: TypeReflection
) extends Reflection {

  def invoke(instance: AnyRef, args: List[Any] = Nil) =
    mirror.invoke(
      instance,
      owner.mt.member(
        mirror.newTermName(name).asInstanceOf[mirror.Name]
      )
    )(args: _*)

  override def toString = owner.toString + "." + name + "(" + argumentTypes.mkString(", ") + "): " + resultType.toString
}
