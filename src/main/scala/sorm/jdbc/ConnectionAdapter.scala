package sorm.jdbc

import java.sql.{Connection, ResultSet, Statement => JdbcStatement}
import com.weiglewilczek.slf4s.Logging

class ConnectionAdapter( connection : Connection ) extends Logging {
  def executeQuery
    ( s : Statement )
    : ResultSet
    = {
      logger.trace("Executing statement:\n" + s.toString)
      preparedStatement(s).executeQuery()
    }

  def executeUpdateAndGetGeneratedKeys
    ( s : Statement )
    : List[IndexedSeq[Any]]
    = {
      logger.trace("Executing statement:\n" + s.toString)
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
      logger.trace("Executing statement:\n" + s.toString)
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