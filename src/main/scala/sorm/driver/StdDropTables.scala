package sorm.driver

import sext._, embrace._
import sorm.jdbc.{JdbcConnection, Statement}

trait StdDropTables { self: StdConnection with StdQuote =>
  def dropTable ( table : String ) {
    table $ ("DROP TABLE " + quote(_)) $ (Statement(_)) $ connection.executeUpdate
  }
}
