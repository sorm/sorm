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
class Connection ( driver : Driver, connection : JdbcConnection ) {
  import abstractSql.AbstractSql._
  def query
    [ T ] 
    ( asql : Statement ) 
    ( parse : Stream[String => Any] => T = (_ : Stream[String => Any]).toList )
    : T
    = connection.executeQuery(driver.statement(asql))(parse)
}
