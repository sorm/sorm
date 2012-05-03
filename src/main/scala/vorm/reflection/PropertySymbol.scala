package vorm.reflection

class PropertySymbol(
  val host: TypeSymbol,
  val name: String,
  t: Type
) extends TypeSymbol(t) {

  val value = host.t.methodResult(name, _: AnyRef)

  override def toString = host.toString + "." + name + ": " + super.toString
}
