package sorm.pooling

import sorm.jdbc.JdbcConnection
import com.weiglewilczek.slf4s.Logging

trait ConnectionPool extends Logging {
  protected def fetchConnection () : JdbcConnection
  protected def returnConnection ( c : JdbcConnection )

  private val openConnectionByThread = collection.mutable.Map[Thread, JdbcConnection]()

  def withConnection [ T ] ( f : JdbcConnection => T ) : T
    = {
      val thread = Thread.currentThread()
      logger.trace("Opening a withConnection block on thread " + thread)
      openConnectionByThread.get(thread) match {
        case Some(cx) =>
          f(cx)
        case None =>
          logger.trace("Fetching a connection to thread " + thread)
          val cx = fetchConnection()
          openConnectionByThread.update(thread, cx)
          try f(cx)
          finally {
            logger.trace("Returning a connection from thread " + thread)
            openConnectionByThread.remove(thread)
            returnConnection(cx)
          }
      }
    }


}
