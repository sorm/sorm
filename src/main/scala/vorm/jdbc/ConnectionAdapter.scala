package vorm.jdbc

import com.weiglewilczek.slf4s.Logging
import java.sql.{Connection, Statement => JdbcStatement}

class ConnectionAdapter
  ( connection : Connection ) 
  extends Logging 
  {

    def executeUpdateAndGetGeneratedKeys
      ( stmt : Statement )
      : List[IndexedSeq[Any]] 
      = {
        if( stmt.data.isEmpty ) {
          val js = connection.createStatement()
          js.executeUpdate(stmt.sql)
          js.getGeneratedKeys.parseAndClose()
        } else {
          val js = preparedStatement(stmt, true)
          js.executeUpdate()
          js.getGeneratedKeys.parseAndClose()
        }
      }

    def executeUpdate
      ( stmt : Statement )
      : Int = {
        if( stmt.data.isEmpty )
          connection.createStatement().executeUpdate(stmt.sql)
        else
          preparedStatement(stmt).executeUpdate()
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