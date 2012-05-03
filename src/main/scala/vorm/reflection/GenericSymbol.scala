package vorm.reflection

class GenericSymbol(
  val host: TypeSymbol,
  val index: Int,
  t: Type
) extends TypeSymbol(t) {

  override def toString = t.toString + "@" + host.toString
}
