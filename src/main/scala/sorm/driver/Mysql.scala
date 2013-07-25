package sorm.driver

import sorm._, ddl._, jdbc._
import sext._, embrace._
import sql.Sql

class Mysql (protected val connection : JdbcConnection)
  extends DriverConnection
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

  override protected def quote ( x : String ) = "`" + x + "`"
  override protected def columnTypeDdl ( t : ColumnType )
    = {
      import ColumnType._
      t match {
        case Text => "LONGTEXT"
        case _ => super.columnTypeDdl(t)
      }
    }
  override protected def postProcessSql(sql: Sql.Statement) = sql
}
