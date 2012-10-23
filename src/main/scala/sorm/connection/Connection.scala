package sorm.connection

import sext._, embrace._
import org.joda.time.DateTime

import sorm._
import sorm.core._
import sorm.abstractSql.AbstractSql._
import sorm.jdbc.ResultSetView

/**
 * An abstraction over jdbc connection, instances of which implement sql dialects of different databases
 */
trait Connection {
  def query
    [ T ]
    ( asql : Statement )
    ( parse : ResultSetView => T = (_ : ResultSetView).indexedRowsTraversable.toList )
    : T
  def now() : DateTime
  def dropTable
    ( table : String )
  def dropAllTables()
  def update
    ( table : String, values : Iterable[(String, Any)], pk : Iterable[(String, Any)] )
  def insert
    ( table : String, values : Iterable[(String, Any)] )
  def insertAndGetGeneratedKeys
    ( table : String, values : Iterable[(String, Any)] )
    : Seq[Any]
  def delete
    ( table : String, pk : Iterable[(String, Any)] )
  def transaction [ T ] ( f : => T ) : T
  def createTable ( table : ddl.Table )
  def close()
}