package sorm.drivers

import org.joda.time.DateTime
import sorm.jdbc.{JdbcConnection, Statement}

trait StdNow {
  protected def connection : JdbcConnection
  def now() : DateTime
    = connection
        .executeQuery(Statement("SELECT NOW()"))()
        .head.head
        .asInstanceOf[DateTime]

}
