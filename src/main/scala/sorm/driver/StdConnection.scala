package sorm.driver

import sorm.jdbc.JdbcConnection

trait StdConnection {
  protected def connection : JdbcConnection
}
