package sorm.mappings

import sext._
import sorm._
import connection.Connection
import core._
import jdbc.ResultSetView

trait Querying {
  import abstractSql.AbstractSql._

  def parseResultSet ( rs : ResultSetView ) : Any

  def containerTableMapping : Option[TableMapping]
  def tableName : String
  def tableColumns : Stream[ddl.Column]
  def connection : Connection
  def bindingsToContainerTable : Stream[(String, String)]
  def primaryKeyColumnNames : Stream[String]

  val fetchByContainerPrimaryKey : Map[String, Any] => Any
    = {
      lazy val table = Table(tableName)
      lazy val containerTable = containerTableMapping.get.tableName $ (Table(_, Parent(table, bindingsToContainerTable.map(_.swap)) $ Some.apply))
      lazy val columns = this.tableColumns.map(_.name).map(Column(_, table))

      ( _
        map {case (n, v) => Comparison(containerTable, n, Equal, v)}
        reduceOption And
        $ ( Select(columns, _) )
        $ ( connection.query(_)(parseResultSet) )
      )
    }

  val fetchByPrimaryKey : Map[String, Any] => Any
    = {
      lazy val table = Table(tableName, None)
      lazy val columns = this.tableColumns.map(_.name).map(Column(_, table))

      ( _
        map {case (n, v) => Comparison(table, n, Equal, v)}
        reduceOption And 
        $ (Select(columns, _))
        $ (connection.query(_)(parseResultSet))
      )
    }

  lazy val abstractSqlTable : Table
    = Table(tableName, containerTableMapping.map(_.abstractSqlTable).map(Parent(_, bindingsToContainerTable)))

  lazy val primaryKeySelect : Select
    = {
      val columns = primaryKeyColumnNames.map(Column(_, abstractSqlTable))
      Select(
        expressions = columns,
        groupBy = columns
      )
    }

}