package sorm.core

import sext.Sext._

import sorm._
import abstractSql._
import abstractSql.AbstractSql._
import jdbc.JdbcValue
import sql._

trait Driver {
  def statement ( asql : Statement ) : jdbc.Statement
    = asql as sql as statement
  def statement ( sql : Sql.Sql ) : jdbc.Statement
    = jdbc.Statement(template(sql), data(sql).map(JdbcValue.apply))
  def sql ( asql : Statement ) : Sql.Statement
  def template ( sql : Sql.Sql ) : String
  def data ( sql : Sql.Sql ) : Seq[Any]
}