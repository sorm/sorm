package sorm.driver

import sorm._, ddl._, jdbc._
import sext._, embrace._
import sorm.sql.Sql

class Postgres (protected val connection : JdbcConnection)
  extends DriverConnection
  with StdConnection
  with StdTransaction
  with StdAbstractSqlToSql
  with StdQuote
  with StdSqlRendering
  with StdStatement
  with StdQuery
  with StdModify
  with StdCreateTable
  with StdListTables
  with StdDropTables
  with StdDropAllTables
  with StdNow
{
  override protected def quote ( x : String ) = "\"" + x + "\""
  override def createTable(table: Table) {
    super.createTable(table)
    table.indexes.foreach{
      createIndexDdl(table.name, _) $ (Statement(_)) $ connection.executeUpdate
    }
  }
  protected def createIndexDdl( table: String, columns: Seq[String] ): String
    = "CREATE INDEX ON " + quote(table) + " (" + columns.view.map(quote).mkString(", ") + ")"
  override protected def indexDdl(columns: Seq[String]) = ""
  override protected def showTablesSql
    = "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"
  override protected def columnDdl(c: Column)
    = if ( !c.autoIncrement ) super.columnDdl(c)
      else quote(c.name) + " BIGSERIAL" + c.nullable.option(" NULL").getOrElse(" NOT NULL")
  override protected def columnTypeDdl ( t : ColumnType )
    = {
      import ColumnType._
      t match {
        case Text => "TEXT"
        case TinyInt => "SMALLINT"
        case Double => "DOUBLE PRECISION"
        case _ => super.columnTypeDdl(t)
      }
    }
  //  dirty: implies that the only thing returned is an id column
  override def insertAndGetGeneratedKeys(table: String, values: Iterable[(String, Any)])
    = super.insertAndGetGeneratedKeys(table, values).take(1)
  override protected def template(sql: Sql) = sql match {
    case Sql.Regexp => "~*"
    case Sql.NotRegexp => "!~*"
    case Sql.Like => "~~*"
    case Sql.NotLike => "!~~*"
    case _ => super.template(sql)
  }
}