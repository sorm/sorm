package vorm

import com.weiglewilczek.slf4s.Logger
import java.sql.{ResultSet, PreparedStatement, Connection, Statement => JStatement}

package object jdbc {
  private lazy val logger = Logger(this.getClass)

  implicit def connectionAdapter(x: Connection) = new ConnectionAdapter(x)
  implicit def preparedStatementAdapter(x: PreparedStatement) = new PreparedStatementAdapter(x)
  implicit def resultSetAdapter(x: ResultSet) = new ResultSetAdapter(x)


  type JdbcType = Int

}