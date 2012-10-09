package sorm.core

import sext.Sext._

import sorm._
import abstractSql.AbstractSql._
import org.joda.time.DateTime

/**
 * An abstraction over jdbc connection, instances of which implement sql dialects of different databases
 */
trait Driver {
  def query
    [ T ] 
    ( asql : Statement ) 
    ( parse : Stream[String => Any] => T = (_ : Stream[String => Any]).toList )
    : T
  def now() : DateTime
  def dropTables
    ( tables : Seq[String] )
  def dropAllTables()
  def update
    ( table : String, values : Map[String, Any], pk : Map[String, Any] )
  def insert
    ( table : String, values : Map[String, Any] )
    : Seq[Any]
  def delete
    ( table : String, pk : Map[String, Any] )
}
object Driver {
  def apply ( url : String, user : String, password : String )
    = DbType.byUrl(url) match {
        case DbType.Mysql => new drivers.Mysql(url, user, password)
        case DbType.H2 => new drivers.H2(url, user, password)
      }
}