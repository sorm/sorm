package sorm.drivers

import sext.Sext._

import sorm._
import abstractSql._
import jdbc._
import sql._

trait StdStatement {
  def statement(asql: AbstractSql.Statement): jdbc.Statement
    = asql $ sql $ statement
  protected def statement(sql: Sql.Sql): jdbc.Statement
  protected def sql(asql: AbstractSql.Statement): Sql.Statement
}
