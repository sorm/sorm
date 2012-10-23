package sorm.connection

import sorm.jdbc.JdbcConnection

class Mysql (protected val connection : JdbcConnection)
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
  with StdClose
