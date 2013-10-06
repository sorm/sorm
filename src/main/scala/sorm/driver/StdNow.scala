package sorm.driver

import org.joda.time.DateTime
import sorm.jdbc.{JdbcConnection, Statement}

trait StdNow { self: StdConnection =>
  def now() : DateTime
    = connection
        .executeQuery(Statement("SELECT NOW()"))()
        .head.head
        .asInstanceOf[DateTime]

}
