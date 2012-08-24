package sorm.dropAll

import sorm._
import jdbc._
import extensions._

trait DropAllTablesAdapter extends ConnectionAdapter {
  def dropAllTables() {
    def listTables()
      = executeQuery( Statement("SHOW TABLES") )
          .parseAndClose()
          .flatten
          .asInstanceOf[List[String]]

    def tryToDrop
      ( table : String )
      {
        try {
          executeUpdate( Statement("DROP TABLE " + table) )
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
      throw new Exception("Couldn't drop all tables")
    }

  }
}
