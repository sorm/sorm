package vorm.jdbc

case class JdbcValue
  ( value : Any,
    t : JdbcType )
object JdbcValue {
  def apply ( v : Any ) : JdbcValue = apply(v, JdbcType(v))
}
