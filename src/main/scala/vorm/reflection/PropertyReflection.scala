package vorm.reflection

import reflect.mirror

class PropertyReflection(val name: String, val owner: TypeReflection, val tpe: TypeReflection) extends Reflection {
  def value(instance: AnyRef) =
    mirror.invoke(instance, owner.mt.member(mirror.newTermName(name).asInstanceOf[mirror.Name]))()

  override def toString = owner.toString + "." + name + ": " + tpe.toString
}
