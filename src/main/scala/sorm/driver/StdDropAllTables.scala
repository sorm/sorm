package sorm.driver

import sorm.jdbc.{JdbcConnection, Statement}
import sorm.core.SormException

trait StdDropAllTables { self: StdConnection with StdListTables with StdQuote =>
  def dropAllTables() {
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
}
