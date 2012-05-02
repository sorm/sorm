package vorm.reflection


class GenericReflection(val index: Int, val owner: TypeReflection, val tpe: TypeReflection) extends Reflection {
  override def toString = tpe.toString + "@" + owner.toString
}
