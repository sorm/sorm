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
      synchronized { openConnectionByThread.get(thread) } match {
        case Some(cx) =>
          f(cx)
        case None =>
          logger.trace("Fetching a connection to thread " + thread)
          val cx = fetchConnection()
          synchronized { openConnectionByThread.update(thread, cx) }
          logger.trace(synchronized { openConnectionByThread.size } + " connections currently fetched")
          try f(cx)
          finally {
            logger.trace("Returning a connection from thread " + thread)
            synchronized { openConnectionByThread.remove(thread) }
            returnConnection(cx)
            logger.trace(synchronized { openConnectionByThread.size } + " connections currently fetched")
          }
      }
    }


}
