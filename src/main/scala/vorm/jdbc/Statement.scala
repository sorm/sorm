package vorm.jdbc

case class Statement(
  sql: String,
  data: Seq[Any] = Nil
) {
  override def toString =
    sql + "\n" + data.mkString(", ")
}