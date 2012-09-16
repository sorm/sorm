package sorm.jdbc

import sext.Sext._
import java.sql.{Connection, ResultSet, Statement => JdbcStatement}
import com.weiglewilczek.slf4s.Logging

trait Transactions {
  protected def connection : Connection
  private var thread : Thread = _
  def transaction [ T ] ( t : => T ) : T
    = synchronized {
        if( !connection.getAutoCommit ){
          assume( Thread.currentThread == thread, "Attempt to get a transaction started on a connection which is already in one but on a different thread" )
          t
        } else {
          var committed = false
          try {
            connection.setAutoCommit(false)
            thread = Thread.currentThread
            val r = t
            connection.commit()
            committed = true
            r
          } finally {
            if( !committed ) connection.rollback()
            connection.setAutoCommit(true)
          }
        }
      }
}
