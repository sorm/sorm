package vorm.jdbc

import java.sql.{Connection, ResultSet, Statement => JdbcStatement}
import vorm.extensions.LogulaLogging

class ConnectionAdapter
  ( connection : Connection )
  extends LogulaLogging
  {
    def executeQuery
      ( s : Statement )
      : ResultSet
      = {
        log.trace("Executing statement:\n" + s.toString)
        preparedStatement(s).executeQuery()
      }

    def executeUpdateAndGetGeneratedKeys
      ( s : Statement )
      : List[IndexedSeq[Any]] 
      = {
        log.trace("Executing statement:\n" + s.toString)
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
        log.trace("Executing statement:\n" + s.toString)
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