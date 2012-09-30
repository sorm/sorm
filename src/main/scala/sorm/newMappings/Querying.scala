package sorm.newMappings

import sext.Sext._
import sorm._
import core._

trait Querying {
  import abstractSql.AbstractSql._

  def parseRows ( rows : Stream[String => Any] ) : Any

  def containerTableMapping : Option[TableMapping]
  def name : String
  def columns : Stream[ddl.Column]
  def connection : Connection
  def bindingsToContainerTable : Stream[(String, String)]

  val fetchByContainerPrimaryKey : Map[String, Any] => Any
    = {
      lazy val containerTable : Option[Table]
        = containerTableMapping map (_.name) map (Table(_))
      lazy val table : Table
        = Table(name, containerTable.map(Parent(_, bindingsToContainerTable)))
      lazy val selectColumns : Stream[Column]
        = columns.map(_.name).map(Column(_, table))

      ( _ 
        map {case (n, v) => Comparison(containerTable.get, n, Equal, v)} 
        reduceOption And
        as ( Select(selectColumns, _) )
        as ( connection.query(_)(parseRows) )
      )
    }

  val fetchByPrimaryKey : Map[String, Any] => Any
    = {
      lazy val table = Table(name, None)
      lazy val columns = this.columns.map(_.name).map(Column(_, table))

      ( _ 
        map {case (n, v) => Comparison(table, n, Equal, v)} 
        reduceOption And 
        as (Select(columns, _))
        as (connection.query(_)(parseRows))
      )
    }
}