package sorm.driver

import sext._, embrace._
import sorm.jdbc.{Statement, JdbcConnection}
import sorm.ddl.Table

class H2 (protected val connection : JdbcConnection)
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
    override protected def tableDdl(t: Table) : String
      = {
        val Table(name, columns, primaryKey, uniqueKeys, indexes, foreingKeys) = t
        val statements =
          columns.map(columnDdl) ++:
          primaryKey.$(primaryKeyDdl) +:
          uniqueKeys.map(uniqueKeyDdl) ++:
          foreingKeys.map(foreingKeyDdl).toStream
        "CREATE TABLE " + quote(name) +
        ( "\n( " + statements.mkString(",\n").indent(2).trim + " )" ).indent(2)
      }
  }