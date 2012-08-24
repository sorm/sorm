package sorm.reflection

case class MethodProperties(
  name: String,
  arguments: List[ArgumentProperties],
  resultType: Type
) {
  override def toString = name + "(" + arguments.mkString(",") + "):" + resultType.toString
}
