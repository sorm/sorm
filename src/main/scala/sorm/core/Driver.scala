package sorm.core

import sext.Sext._

import sorm._
import abstractSql._
import jdbc._

/**
 * An abstraction over jdbc connection, instances of which implement sql dialects of different databases
 * 
 * Probably should be renamed to Connection, while the Renderer - to Driver and ConnectionAdapter - to JdbcConnection
 */
class Driver ( renderer : Renderer, connection : ConnectionAdapter ) {
  import abstractSql.AbstractSql._
  def query
    [ T ] 
    ( asql : Statement ) 
//    ( parse : Stream[Map[String, _]] => T = (_ : Stream[Map[String, _]]).toList )
    ( parse : Stream[Map[String, _]] => T = (_.toList) )
    : T
    = (asql as renderer.sql as connection.executeQuery)(parse)
}
