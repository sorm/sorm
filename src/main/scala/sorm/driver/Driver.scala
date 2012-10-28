package sorm.driver

import sext._, embrace._
import java.sql.DriverManager

import sorm._
import core._
import jdbc._

trait Driver {
  def connection () : DriverConnection

  def withTmpConnection [ T ] ( f : DriverConnection => T ) = {
    val c = connection()
    try { f(c) } finally { c.close() }
  }
}
object Driver {
  def apply ( url : String, user : String, password : String )
    = {
      def jdbcConnection()
        = {
        val c = DriverManager.getConnection(url, user, password)
//        completely chokes the performance
//        c.setTransactionIsolation(java.sql.Connection.TRANSACTION_SERIALIZABLE)
        new JdbcConnection(c)
      }

      val dbType = DbType.byUrl(url)

      //  preload driver
      dbType $ DbType.driverClass $ Class.forName
      
      dbType match {
        case DbType.Mysql => 
          new Driver {
            def connection () = new Mysql(jdbcConnection())
          }
        case DbType.H2 => 
          new Driver {
            val connection = new H2(jdbcConnection())
            override def withTmpConnection [ T ] ( f : DriverConnection => T ) = f(connection)
          }
        case _ => throw new SormException("Unsupported db type: " + dbType)
      }
    }
}