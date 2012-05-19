package vorm

import com.weiglewilczek.slf4s.Logger
import java.sql.{PreparedStatement, Connection, Statement => JStatement}
import org.joda.time.DateTime

package object jdbc {
  private lazy val logger = Logger(this.getClass)

  implicit def connectionAPI(x: Connection) = new ConnectionAPI(x)
  implicit def preparedStatementAPI(x: PreparedStatement) = new PreparedStatementAPI(x)



}