package sorm.drivers

import sorm.core.Driver
import sorm.jdbc.JdbcConnection

class Mysql (url:String, user:String, password:String)
  extends Driver
  with StdQuery
  with StdSqlRendering
  with StdDropAllTables
  with StdAbstractSqlToSql
  with StdNow
  with StdModify
  with StdDropTables
  with StdQuote
  with StdTransaction
  with StdCreateTable
{
  val connection = JdbcConnection(url, user, password)
}