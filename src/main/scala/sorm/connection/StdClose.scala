package sorm.connection

import sorm.jdbc.JdbcConnection

trait StdClose {
  protected def connection: JdbcConnection
  def close() = connection.close()
}
