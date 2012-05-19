package vorm.jdbc

import com.weiglewilczek.slf4s.Logging
import java.sql.{ResultSet, PreparedStatement, Connection, Statement => JStatement}
import org.joda.time.DateTime

class ConnectionAPI(connection: Connection) extends Logging {


  def executeUpdateAndGetGeneratedKeys(stmt: Statement) = {
    if (stmt.data.isEmpty) {
      val js = connection.createStatement()
      js.executeUpdate(stmt.sql).ensuring(_ == 1)
      js.getGeneratedKeys
    } else {
      val js = preparedStatement(stmt, true)
      js.executeUpdate().ensuring(_ == 1)
      js.getGeneratedKeys
    }
  }
  def executeUpdate(stmt: Statement) = {
    if (stmt.data.isEmpty) connection.createStatement().executeUpdate(stmt.sql)
    else preparedStatement(stmt).executeUpdate()
  }

  private def preparedStatement(stmt: Statement, generatedKeys: Boolean = false) = {
    val s =
      connection.prepareStatement(
        stmt.sql,
        if (generatedKeys) JStatement.RETURN_GENERATED_KEYS
        else JStatement.NO_GENERATED_KEYS
      )
    stmt.data.zipWithIndex.foreach {
      case (v, i) => s.set(i + 1, v)
    }

    s
  }


}