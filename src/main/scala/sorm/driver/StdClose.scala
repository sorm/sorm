package sorm.driver

import sorm.jdbc.JdbcConnection

trait StdClose {
  protected def connection: JdbcConnection
  def close() = connection.close()
}
