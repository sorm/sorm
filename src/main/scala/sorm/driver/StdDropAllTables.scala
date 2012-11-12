package sorm.driver

import sorm.jdbc.{JdbcConnection, Statement}
import sorm.core.SormException

trait StdDropAllTables {
  protected def connection: JdbcConnection
  protected def quote ( a : String ) : String
  def dropAllTables() {
    def listTables()
      = connection.executeQuery( Statement(showTablesSql) )()
          .flatten
          .asInstanceOf[List[String]]

    def tryToDrop
      ( table : String )
      {
        try {
          connection.executeUpdate( Statement("DROP TABLE " + quote(table)) )
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
  protected def showTablesSql : String = "SHOW TABLES"

}
