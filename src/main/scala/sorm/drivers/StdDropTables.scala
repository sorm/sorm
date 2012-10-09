package sorm.drivers

import sorm.jdbc.{JdbcConnection, Statement}
import sorm.core.SormException

trait StdDropTables {
  protected def connection : JdbcConnection
  protected def quote ( x : String ) : String
  def dropTables ( tables : Seq[String] ) {
    tables
      .view
      .map("DROP TABLE " + quote(_))
      .map(Statement(_))
      .foreach(connection.executeUpdate)
  }

}
