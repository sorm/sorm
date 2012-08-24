package sorm.reflection

case class PropertyProperties(
  name: String,
  t: Type
) {
  override def toString = name + ":" + t
}
