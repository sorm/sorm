package vorm

import com.weiglewilczek.slf4s.Logger
import java.sql.{PreparedStatement, Connection, Statement => JStatement}
import org.joda.time.DateTime

package object jdbc {
  private lazy val logger = Logger(this.getClass)

  implicit def connectionAPI(x: Connection) = new ConnectionAPI(x)

  /**
   * @see <a href=http://docstore.mik.ua/orelly/java-ent/servlet/ch09_02.htm#ch09-22421>jdbc table
   */
  def setStmtVar(s: PreparedStatement, v: Any, i: Int) {
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
    }
  }

}