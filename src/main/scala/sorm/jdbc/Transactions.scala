package sorm.jdbc

import sext._, embrace._
import java.sql.{Statement => JdbcStatement, SQLTransactionRollbackException, Connection, ResultSet}
import reflect.ClassTag

trait Transactions {
  protected def connection : Connection

  private var thread : Thread = _
  
  def transaction [ T ] ( t : => T ) : T
    = transaction(100)(t)

  private def transaction [ T ] ( attempts : Int ) ( t : => T ) : T
    = synchronized {
        if( !connection.getAutoCommit ){
          assume( Thread.currentThread == thread, "Attempt to get a transaction started on a connection which is already in one but on a different thread" )
          t
        } else {
          try {
            connection.setAutoCommit(false)
            thread = Thread.currentThread
            val r = t
            connection.commit()
            connection.setAutoCommit(true)
            r
          } catch {
            case e : SQLTransactionRollbackException if attempts > 0 =>
              connection.rollback()
              connection.setAutoCommit(true)
              transaction(attempts - 1)(t)
            case e : Throwable =>
              connection.rollback()
              connection.setAutoCommit(true)
              throw e
          }
        }
      }

}
