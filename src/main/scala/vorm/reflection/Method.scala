package vorm.reflection

case class Method(
  name: String,
  arguments: List[Argument],
  resultType: Type
) {
  override def toString = name + "(" + arguments.mkString(", ") + "): " + resultType.toString
}
