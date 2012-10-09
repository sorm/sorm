package sorm.drivers

import sext.Sext._

import sorm._
import jdbc._

trait StdConnection {
  protected def connection: JdbcConnection

  import abstractSql.AbstractSql._
  def query
    [ T ]
    ( asql : Statement )
    ( parse : Stream[String => Any] => T = (_ : Stream[String => Any]).toList )
    : T
    = connection.executeQuery(statement(asql))(parse)

  protected def statement ( asql : Statement ) : jdbc.Statement

}
