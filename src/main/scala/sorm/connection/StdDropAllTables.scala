package sorm.connection

import sorm.jdbc.{JdbcConnection, Statement}
import sorm.core.SormException

trait StdDropAllTables {
  protected def connection: JdbcConnection
  def dropAllTables() {
    def listTables()
      = connection.executeQuery( Statement("SHOW TABLES") )()
          .flatten
          .asInstanceOf[List[String]]

    def tryToDrop
      ( table : String )
      {
        try {
          connection.executeUpdate( Statement("DROP TABLE " + table) )
        } catch {
          case e : Throwable =>
        }
      }

    var lastTables = List[String]()
    var tables = listTables()
    while( tables != lastTables ) {
      tables foreach tryToDrop
      lastTables = tables
      tables = listTables()
    }

    if( !lastTables.isEmpty ) {
      throw new SormException("Couldn't drop all tables")
    }

  }

}
