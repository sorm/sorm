package sorm.drivers

import org.joda.time.DateTime
import sorm.jdbc.{JdbcConnection, Statement}

trait StdTransaction {
  protected def connection : JdbcConnection

  def transaction [ T ] ( t : => T ) : T = connection.transaction(t)
}
