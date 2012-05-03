package vorm.reflection

class MethodSymbol(
  val host: TypeSymbol,
  val name: String,
  val argumentTypes: Map[String, Type],
  val resultType: Type
) extends Symbol {

  val result = host.t.methodResult(name, _:AnyRef, _: List[Any])

  override def toString =
    host.toString+"."+name+"("+argumentTypes.mkString(", ")+"): "+resultType.toString
}
