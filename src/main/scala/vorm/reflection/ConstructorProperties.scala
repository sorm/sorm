package vorm.reflection

case class ConstructorProperties(
  arguments: List[ArgumentProperties]
) {
  override def toString = "constructor(" + arguments.mkString(",") + ")"
}