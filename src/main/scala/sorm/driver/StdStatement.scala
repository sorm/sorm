package sorm.driver

import sorm.sql.Sql
import sorm.jdbc

trait StdStatement {
  def statement ( sql : Sql ) : jdbc.Statement
}
