package sorm.pooling

import sext._, embrace._
import com.mchange.v2.c3p0.ComboPooledDataSource

import sorm.core.DbType
import sorm.jdbc.JdbcConnection

class C3p0ConnectionPool (dbType: DbType, url: String, user: String, password: String, size: Int, timeout: Int) extends ConnectionPool {

  private val ds = new ComboPooledDataSource()
  ds.setDriverClass(dbType $ DbType.driverClass)
  ds.setJdbcUrl(url)
  ds.setUser(user)
  ds.setPassword(password)
  ds.setMinPoolSize(0)
  ds.setMaxPoolSize(size)

  // Connection timeout. For more info see:
  // http://www.mchange.com/projects/c3p0/#configuring_connection_testing
  ds.setIdleConnectionTestPeriod(timeout)


  protected def fetchConnection () = ds.getConnection $ (new JdbcConnection(_))
  protected def returnConnection ( c : JdbcConnection ) = c.close()

  def close() = ds.close()
}
