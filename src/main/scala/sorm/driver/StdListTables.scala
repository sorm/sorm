package sorm.driver

import sorm.jdbc.Statement

trait StdListTables { self: StdConnection =>
  protected def showTablesSql : String = "SHOW TABLES"

  def listTables(): List[String] =
    connection.executeQuery( Statement(showTablesSql) )()
      .flatten
      .asInstanceOf[List[String]]
}
