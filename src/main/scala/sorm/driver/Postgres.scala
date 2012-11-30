package sorm.driver

import sorm._, ddl._, jdbc._
import sext._, embrace._

class Postgres (protected val connection : JdbcConnection)
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
        case _ => super.columnTypeDdl(t)
      }
    }
  //  dirty: implies that the only thing returned is an id column
  override def insertAndGetGeneratedKeys(table: String, values: Iterable[(String, Any)])
    = super.insertAndGetGeneratedKeys(table, values).take(1)
}