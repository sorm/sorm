package vorm.reflection

case class Property(
  name: String,
  t: Type
) {
  override def toString = name + ":" + t
}