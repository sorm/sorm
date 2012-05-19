package vorm.jdbc

import java.sql.PreparedStatement
import org.joda.time.DateTime

class PreparedStatementAPI (s: PreparedStatement) {
  /**
   * @see <a href=http://docstore.mik.ua/orelly/java-ent/servlet/ch09_02.htm#ch09-22421>jdbc table
   */
  def set(i: Int, v: Any) {
    v match {
      case v: Boolean              => s.setBoolean(i, v)
      case v: String               => s.setString(i, v)
      case v: Int                  => s.setInt(i, v)
      case v: Integer              => s.setInt(i, v)
      case v: Long                 => s.setLong(i, v)
      case v: Float                => s.setFloat(i, v)
      case v: Double               => s.setDouble(i, v)
      case v: BigDecimal           => s.setBigDecimal(i, v.bigDecimal)
      case v: java.math.BigDecimal => s.setBigDecimal(i, v)
      case v: DateTime             => s.setDate(i, new java.sql.Date(v.getMillis))
      case Some(v)                 => set(i, v)
      case None                    => s.setNull(i, java.sql.Types.NULL)
    }
  }
}
