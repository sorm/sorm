package sorm.pooling

import sext._, embrace._
import com.mchange.v2.c3p0.ComboPooledDataSource

import sorm.core.DbType
import sorm.jdbc.JdbcConnection

class C3p0ConnectionPool (dbType: DbType, url: String, user: String, password: String, size: Int) extends ConnectionPool {

  private val ds = new ComboPooledDataSource()
  ds.setDriverClass(dbType $ DbType.driverClass)
  ds.setJdbcUrl(url)
  ds.setUser(user)
  ds.setPassword(password)
  ds.setMinPoolSize(1)
  ds.setMaxPoolSize(size)
//  ds.setMaxStatements(20)
//  ds.setAcquireIncrement(3)
//  ds.setStatementCacheNumDeferredCloseThreads(4)

  protected def fetchConnection () = ds.getConnection $ (new JdbcConnection(_))
  protected def returnConnection ( c : JdbcConnection ) = c.close()
}
