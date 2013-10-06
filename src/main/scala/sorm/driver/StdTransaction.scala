package sorm.driver

import sorm.jdbc.JdbcConnection

trait StdTransaction { self: StdConnection =>
  def transaction [ T ] ( t : => T ) : T = connection.transaction(t)
}
