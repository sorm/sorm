package sorm.drivers

import sorm.core.Driver
import sorm.jdbc.JdbcConnection

class Mysql (url:String, user:String, password:String)
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
}