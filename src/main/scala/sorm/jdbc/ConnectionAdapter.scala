package sorm.jdbc

import sext.Sext._
import java.sql.{Connection, ResultSet, Statement => JdbcStatement}
import com.weiglewilczek.slf4s.Logging

class ConnectionAdapter( protected val connection : Connection ) extends Transactions with Logging {
  private def logStatement(s : Statement){
    logger.trace(
      "Executing statement:\n" +
      Map("sql" -> s.sql, "data" -> s.data.map(_.value)).valueTreeString
    )
  }
  def executeQuery
    ( s : Statement )
    : ResultSet
    = {
      logStatement(s)
      preparedStatement(s).executeQuery()
    }

  def executeUpdateAndGetGeneratedKeys
    ( s : Statement )
    : List[IndexedSeq[Any]]
    = {
      logStatement(s)
      if( s.data.isEmpty ) {
        val js = connection.createStatement()
        js.executeUpdate(s.sql, JdbcStatement.RETURN_GENERATED_KEYS)
        js.getGeneratedKeys.parseAndClose()
      } else {
        val js = preparedStatement(s, true)
        js.executeUpdate()
        js.getGeneratedKeys.parseAndClose()
      }
    }

  def executeUpdate
    ( s : Statement )
    : Int = {
      logStatement(s)
      if( s.data.isEmpty ){
        connection.createStatement().executeUpdate(s.sql)
      } else {
        preparedStatement(s).executeUpdate()
      }
    }

  private def preparedStatement
    ( stmt : Statement,
      generatedKeys : Boolean = false )
    = {
      val s =
        connection.prepareStatement(
          stmt.sql,
          if (generatedKeys) JdbcStatement.RETURN_GENERATED_KEYS
          else JdbcStatement.NO_GENERATED_KEYS
        )

      for( (v, i) <- stmt.data.view.zipWithIndex ) {
        s.set(i + 1, v)
      }

      s
    }

}