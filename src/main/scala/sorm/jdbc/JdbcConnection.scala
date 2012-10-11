package sorm.jdbc

import java.sql.DriverManager
import sext.Sext._
import java.sql.{Connection, ResultSet, Statement => JdbcStatement}
import com.weiglewilczek.slf4s.Logging
import sorm.core.DbType

class JdbcConnection( protected val connection : Connection ) extends Transactions with Logging {
  private def logStatement(s : Statement){
    logger.trace(
      "Executing statement:\n" +
      (("sql" -> s.sql) +: s.data.map(_.value).notEmpty.map("data" -> _) ++: Stream())
        .toMap.valueTreeString
    )
  }
  def executeQuery
    [ T ]
    ( s : Statement )
    ( parse : ResultSetView => T = (_ : ResultSetView).indexedRowsTraversable.toList )
    : T
    = {
      logStatement(s)
      val js = preparedStatement(s)
      val rs = js.executeQuery()
      val r = parse(rs)
      rs.close()
      js.close()
      r
    }

  def executeUpdateAndGetGeneratedKeys
    ( s : Statement )
    : List[IndexedSeq[Any]]
    = {
      logStatement(s)
      if( s.data.isEmpty ) {
        val js = connection.createStatement()
        js.executeUpdate(s.sql, JdbcStatement.RETURN_GENERATED_KEYS)
        val rs = js.getGeneratedKeys
        val r = rs.indexedRowsTraversable.toList
        rs.close()
        js.close()
        r
      } else {
        val js = preparedStatement(s, true)
        js.executeUpdate()
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
      logStatement(s)
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
object JdbcConnection {

  def apply
    ( url : String,
      user : String,
      password : String )
    = {
      //  preload driver
      DbType.byUrl(url) match {
        case DbType.Mysql =>
          Class.forName("com.mysql.jdbc.Driver")
        case DbType.Postgres =>
          Class.forName("org.postgresql.Driver")
        case DbType.H2 =>
          Class.forName("org.h2.Driver")
        case DbType.Sqlite =>
          Class.forName("org.sqlite.JDBC")
        case DbType.Hsqldb =>
          Class.forName("org.hsqldb.jdbcDriver")
        case _ =>
          ???
      }
      //  get it
      DriverManager.getConnection(url, user, password) $ (new JdbcConnection(_))
    }


}
