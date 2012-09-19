package sorm.jdbc

import sext.Sext._
import java.sql.{Statement => JdbcStatement, SQLTransactionRollbackException, Connection, ResultSet}
import com.weiglewilczek.slf4s.Logging
import reflect.ClassTag

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
//            retry[SQLTransactionRollbackException, T](50, 1){
//              connection.setAutoCommit(false)
//              thread = Thread.currentThread
//              val r = t
//              connection.commit()
//              committed = true
//              r
//            }
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


  private def retry [ E <: Throwable : ClassTag, T ] ( times : Int, interval : Long = 0 ) ( f : => T ) : T
    = try f
      catch {
        case e : E =>
          if( times > 0 ) {
            if( interval > 0l ) Thread.sleep(interval)
            retry(times - 1, interval)(f)
          }
          else throw e
      }

}
