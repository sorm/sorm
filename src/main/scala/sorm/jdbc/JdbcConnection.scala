package sorm.jdbc

import java.sql.DriverManager
import sext._, embrace._
import java.sql.{Connection, ResultSet, Statement => JdbcStatement}
import sorm.core.DbType

class JdbcConnection( protected val connection : Connection ) extends Transactions with JdbcConnectionLogging {

  def executeQuery
    [ T ]
    ( s : Statement )
    ( parse : ResultSetView => T = (_ : ResultSetView).indexedRowsTraversable.toList )
    : T
    = {
      val js = preparedStatement(s)
      val rs = log(s)(js.executeQuery())
      val r = parse(rs)
      rs.close()
      js.close()
      r
    }

  def executeUpdateAndGetGeneratedKeys
    ( s : Statement )
    : List[IndexedSeq[Any]]
    = {
      if( s.data.isEmpty ) {
        val js = connection.createStatement()
        log(s)(js.executeUpdate(s.sql, JdbcStatement.RETURN_GENERATED_KEYS))
        val rs = js.getGeneratedKeys
        val r = rs.indexedRowsTraversable.toList
        rs.close()
        js.close()
        r
      } else {
        val js = preparedStatement(s, true)
        log(s)(js.executeUpdate())
        val rs = js.getGeneratedKeys
        val r = rs.indexedRowsTraversable.toList
        rs.close()
        js.close()
        r
      }
    }

  def executeUpdate
    ( s : Statement )
    : Int
    = {
      if( s.data.isEmpty ){
        val js = connection.createStatement()
        val r = log(s)(js.executeUpdate(s.sql))
        js.close()
        r
      } else {
        val js = preparedStatement(s)
        val r = log(s)(js.executeUpdate())
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

  def close() = connection.close()
}
object JdbcConnection {

  def apply
    ( url : String,
      user : String,
      password : String )
    = {
      //  preload driver
      url $ DbType.byUrl $ DbType.driverClass $ Class.forName

      //  get it
      DriverManager.getConnection(url, user, password) $ (new JdbcConnection(_))
    }


}
