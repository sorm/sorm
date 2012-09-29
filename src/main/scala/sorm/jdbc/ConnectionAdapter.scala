package sorm.jdbc

import sext.Sext._
import java.sql.{Connection, ResultSet, Statement => JdbcStatement}
import com.weiglewilczek.slf4s.Logging

class ConnectionAdapter( protected val connection : Connection ) extends Transactions with Logging {
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
    ( parse : ResultSet => T = ((_ : ResultSet).parse()) )
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
        val r = rs.parse()
        rs.close()
        js.close()
        r
      } else {
        val js = preparedStatement(s, true)
        js.executeUpdate()
        val rs = js.getGeneratedKeys
        val r = rs.parse()
        rs.close()
        js.close()
        r
      }
    }

  def executeUpdate
    ( s : Statement )
    : Int = {
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