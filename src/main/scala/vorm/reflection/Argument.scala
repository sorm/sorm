package vorm.reflection

case class Argument(
  name: String,
  t: Type
) {
  override def toString = name + ":" + t
}