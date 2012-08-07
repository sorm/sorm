package vorm

import com.weiglewilczek.slf4s.Logger
import java.sql.{ResultSet, PreparedStatement, Connection, Statement => JStatement}

package object jdbc {
  private lazy val logger = Logger(this.getClass)

  implicit def connectionAPI(x: Connection) = new ConnectionAPI(x)
  implicit def preparedStatementAPI(x: PreparedStatement) = new PreparedStatementAPI(x)
  implicit def resultSetAPI(x: ResultSet) = new ResultSetAPI(x)


  type JdbcType = Int

}