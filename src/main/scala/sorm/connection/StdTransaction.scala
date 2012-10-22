package sorm.connection

import sorm.jdbc.JdbcConnection

trait StdTransaction {
  protected def connection : JdbcConnection

  def transaction [ T ] ( t : => T ) : T = connection.transaction(t)
}
