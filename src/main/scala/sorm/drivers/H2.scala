package sorm.drivers

import sext.Sext._
import sorm.core.Driver
import sorm.jdbc.JdbcConnection
import sorm.ddl.Table

class H2 (url:String, user:String, password:String)
  extends Driver
  with StdConnection
  with StdSqlRendering
  with StdDropAllTables
  with StdStatement
  with StdAbstractSqlToSql
  with StdNow
  with StdSaving
  with StdDropTables
  with StdQuote
  with StdTransaction
  with StdCreateTable
{
  def connection = JdbcConnection(url, user, password)
  override protected def tableDdl(t: Table) : String
    = {
      val Table(name, columns, primaryKey, uniqueKeys, indexes, foreingKeys) = t
      val statements =
        columns.map(columnDdl) ++:
        primaryKey.$(primaryKeyDdl) +:
        uniqueKeys.map(uniqueKeyDdl) ++:
        foreingKeys.map(foreingKeyDdl).toStream
      "CREATE TABLE " + quote(name) +
      ( "( " + statements.mkString(",\n").indent(2).trim + " )" ).indent(2)
    }
}