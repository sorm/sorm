package sorm.reflection

case class ArgumentProperties(
  name: String,
  t: Type
) {
  override def toString = name + ":" + t
}
