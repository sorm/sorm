package vorm.jdbc

import com.weiglewilczek.slf4s.Logging
import java.sql.{Connection, Statement => JStatement}

class ConnectionAPI(connection: Connection) extends Logging {

  def executeUpdateAndGetGeneratedKeys(stmt: Statement) = {
    if (stmt.data.isEmpty) {
      val js = connection.createStatement()
      js.executeUpdate(stmt.sql)
      js.getGeneratedKeys.parseToListsAndClose()
    } else {
      val js = preparedStatement(stmt, true)
      js.executeUpdate()
      js.getGeneratedKeys.parseToListsAndClose()
    }
  }

  def executeUpdate(stmt: Statement) = {
    if (stmt.data.isEmpty)
      connection.createStatement().executeUpdate(stmt.sql)
    else
      preparedStatement(stmt).executeUpdate()
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