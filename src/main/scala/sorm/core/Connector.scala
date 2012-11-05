package sorm.core

import sorm._
import core._
import jdbc._
import driver._

import sext._, embrace._

/**
 * Abstracts connections pooling away by binding connections to threads
 */
class Connector (url: String, user: String, password: String, poolSize: Int) {
  private val dbType = DbType.byUrl(url)
  private val pool = new C3p0JdbcConnectionPool(dbType, url, user, password, poolSize)
  private val boundConnections = collection.mutable.Map[Thread, JdbcConnection]()

  /**
   * Get a connection bound to current thread
   */
  private def fetchConnection () : JdbcConnection = {
    boundConnections.getOrElseUpdate(Thread.currentThread(), pool.fetchConnection())
  }
  private def returnConnection () {
    boundConnections.remove(Thread.currentThread())
      .foreach(pool.returnConnection)
  }
  private def driverConnection ( jdbcConnection : JdbcConnection ) : DriverConnection
    = dbType match {
        case DbType.H2 => new H2(jdbcConnection)
        case DbType.Mysql => new Mysql(jdbcConnection)
        case _ => ???
      }
  
  def withConnection [ T ] ( f : DriverConnection => T ) : T
    = try { fetchConnection() $ driverConnection $ f }
      finally { returnConnection() }

  // def perform [ T ] ( f : DriverConnection => T ) : T = {

  // }
  // def performInTransaction [ T ] ( f : DriverConnection => T ) : T = {

  // }

}