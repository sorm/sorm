package sorm.core

import sorm._
import core._
import jdbc._
import driver._
import pooling._

import sext._, embrace._

/**
 * Abstracts connections pooling away by binding connections to threads
 */
class Connector (url: String, user: String, password: String, poolSize: Int, timeout: Int) {
  private val dbType = DbType.byUrl(url)
  private val pool = new C3p0ConnectionPool(dbType, url, user, password, poolSize, timeout)
  private def driverConnection ( jdbcConnection : JdbcConnection ) : DriverConnection
    = dbType match {
        case DbType.H2 => new H2(jdbcConnection)
        case DbType.Mysql => new Mysql(jdbcConnection)
        case DbType.Hsqldb => new Hsqldb(jdbcConnection)
        case DbType.Postgres => new Postgres(jdbcConnection)
        case DbType.Oracle => new Oracle(jdbcConnection)
        case _ => ???
      }
  
  def withConnection [ T ] ( f : DriverConnection => T ) : T
    = pool.withConnection( _ $ driverConnection $ f )

  def close() = pool.close()
}