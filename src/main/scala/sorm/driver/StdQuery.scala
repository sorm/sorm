package sorm.driver

import sext._, embrace._

import sorm._
import jdbc._
import sql._

trait StdQuery { self: StdConnection with StdStatement =>
  import abstractSql.AbstractSql._
  def query
    [ T ]
    ( asql : Statement )
    ( parse : ResultSetView => T = (_ : ResultSetView).indexedRowsTraversable.toList )
    : T
    = queryJdbc(statement(asql))(parse)
  def queryJdbc
    [ T ]
    ( s : jdbc.Statement )
    ( parse : ResultSetView => T = (_ : ResultSetView).indexedRowsTraversable.toList )
    : T
    = connection.executeQuery(s)(parse)

  protected def statement(asql: Statement): jdbc.Statement
    = asql $ sql $ postProcessSql $ Optimization.optimized $ statement
  protected def sql(asql: Statement): Sql.Statement
  protected def postProcessSql(sql: Sql.Statement): Sql.Statement = {
    def includeOrderInWhat(sql: Sql.Select) = {
      val orderByColumns = sql.orderBy.map(_.what)
      val whatColumns = sql.what.collect{ case c: Sql.Column => c }
      val columnsToAdd = orderByColumns.diff(whatColumns)
      sql.copy(
        what = sql.what ++ columnsToAdd,
        groupBy = 
          if( sql.what.diff(sql.groupBy) == Nil )
            sql.groupBy ++ columnsToAdd
          else
            sql.groupBy
      )
    }

    def processSelects( sql: Sql.Statement, f: Sql.Select => Sql.Select )
      : Sql.Statement
      = sql match {
          case Sql.Union(l, r) =>
            Sql.Union(processSelects(l, f), processSelects(r, f))
          case s: Sql.Select =>
            f(s).copy(
              join = s.join.map{
                case j @ Sql.Join(what: Sql.Statement, _, _, _) =>
                  j.copy(processSelects(what, f))
                case j => j
              }
            )
        }

    processSelects(sql, includeOrderInWhat)
  }

}
