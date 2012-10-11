package sorm.mappings

import sext.Sext._
import sorm._
import core._
import jdbc.ResultSetView

trait Querying {
  import abstractSql.AbstractSql._

  def parseResultSet ( rs : ResultSetView ) : Any

  def containerTableMapping : Option[TableMapping]
  def tableName : String
  def columns : Stream[ddl.Column]
  def driver : Driver
  def bindingsToContainerTable : Stream[(String, String)]
  def primaryKeyColumnNames : Stream[String]

  val fetchByContainerPrimaryKey : Map[String, Any] => Any
    = {
      lazy val containerTable : Option[Table]
        = containerTableMapping map (_.tableName) map (Table(_))
      lazy val table : Table
        = Table(tableName, containerTable.map(Parent(_, bindingsToContainerTable)))
      lazy val columns : Stream[Column]
        = columns.map(_.name).map(Column(_, table))

      ( _
        map {case (n, v) => Comparison(containerTable.get, n, Equal, v)} 
        reduceOption And
        $ ( Select(columns, _) )
        $ ( driver.query(_)(parseResultSet) )
      )
    }

  val fetchByPrimaryKey : Map[String, Any] => Any
    = {
      lazy val table = Table(tableName, None)
      lazy val columns = this.columns.map(_.name).map(Column(_, table))

      ( _
        map {case (n, v) => Comparison(table, n, Equal, v)}
        reduceOption And 
        $ (Select(columns, _))
        $ (driver.query(_)(parseResultSet))
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