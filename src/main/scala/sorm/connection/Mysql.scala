package sorm.connection

import sorm.jdbc.JdbcConnection

class Mysql (url:String, user:String, password:String)
  extends Connection
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