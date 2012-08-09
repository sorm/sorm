package vorm.jdbc

import java.sql.{Connection, ResultSet, Statement => JdbcStatement}
import com.codahale.logula.Logging

class ConnectionAdapter
  ( connection : Connection ) 
  extends Logging
  {
    def executeQuery
      ( s : Statement )
      : ResultSet
      = {
        log.info("Executing statement:\n" + s.toString)
        val js = preparedStatement(s)
        val rs = js.executeQuery()
        js.close()
        rs
      }

    def executeUpdateAndGetGeneratedKeys
      ( s : Statement )
      : List[IndexedSeq[Any]] 
      = {
        log.info("Executing statement:\n" + s.toString)
        if( s.data.isEmpty ) {
          val js = connection.createStatement()
          js.executeUpdate(s.sql, JdbcStatement.RETURN_GENERATED_KEYS)
          val r = js.getGeneratedKeys.parseAndClose()
          js.close()
          r
        } else {
          val js = preparedStatement(s, true)
          js.executeUpdate()
          val r = js.getGeneratedKeys.parseAndClose()
          js.close()
          r
        }
      }

    def executeUpdate
      ( s : Statement )
      : Int = {
        log.info("Executing statement:\n" + s.toString)
        if( s.data.isEmpty ){
          val js = connection.createStatement()
          val r = js.executeUpdate(s.sql)
          js.close()
          r
        } else {
          val js = preparedStatement(s)
          val r = js.executeUpdate()
          js.close()
          r
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