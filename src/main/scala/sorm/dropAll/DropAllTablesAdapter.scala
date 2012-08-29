package sorm.dropAll

import sorm._
import core.SormException
import jdbc._
import extensions.Extensions._

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
      throw new SormException("Couldn't drop all tables")
    }

  }
}
