package sorm.newMappings

import sext.Sext._
import sorm._
import core._

trait TableMappingQuerying extends TableMapping {
  import abstractSql.AbstractSql._

  protected def parseRows ( rows : Stream[Map[String, _]] ) : Option[_]

  // private lazy val containerTable : Option[Table]
  //   = containerTableMapping map (_.tableName) map (Table(_))
  // private lazy val table : Table
  //   = Table(tableName, containerTable.map(Parent(_, bindingsToContainerTable)))
  // private lazy val selectColumns : Stream[Column]
  //   = columns.map(_.name).map(Column(_, table))
  // private lazy val bindingsToContainerTable : Stream[(String, String)]
  //   = masterTableForeignKey match {
  //       case Some(fk) => 
  //         fk.bindings.toStream
  //       case None =>
  //         foreignKeyForContainer.toStream flatMap (_.bindings.toStream) map (_.swap)
  //     }
  // def fetchByContainerPrimaryKey ( primaryKey : Map[String, _] ) : _
  //   = primaryKey map {case (n, v) => Comparison(containerTable.get, n, Equal, v)} reduceOption And as
  //     ( Select(selectColumns, _) ) as
  //     ( driver.query(_)(parseRows) )

  val fetchByContainerPrimaryKey : Map[String, _] => Option[_]
    = {
      lazy val containerTable : Option[Table]
        = containerTableMapping map (_.tableName) map (Table(_))
      lazy val table : Table
        = Table(tableName, containerTable.map(Parent(_, bindingsToContainerTable)))
      lazy val selectColumns : Stream[Column]
        = columns.map(_.name).map(Column(_, table))
      lazy val bindingsToContainerTable : Stream[(String, String)]
        = masterTableForeignKey match {
            case Some(fk) => 
              fk.bindings.toStream
            case None =>
              foreignKeyForContainer.toStream flatMap (_.bindings.toStream) map (_.swap)
          }
      ( _ 
        map {case (n, v) => Comparison(containerTable.get, n, Equal, v)} 
        reduceOption And
        as ( Select(selectColumns, _) )
        as ( driver.query(_)(parseRows) )
      )
    }


  val fetchByPrimaryKey : Map[String, _] => Option[_]
    = {
      lazy val table = Table(tableName, None)
      lazy val columns = this.columns.map(_.name).map(Column(_, table))

      ( _ 
        map {case (n, v) => Comparison(table, n, Equal, v)} 
        reduceOption And 
        as (Select(columns, _))
        as (driver.query(_)(parseRows))
      )
    }
}