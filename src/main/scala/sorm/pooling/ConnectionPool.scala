package sorm.pooling

import sorm.jdbc.JdbcConnection

trait ConnectionPool {
  protected def fetchConnection () : JdbcConnection
  protected def returnConnection ( c : JdbcConnection )

  private val openConnectionByThread = collection.mutable.Map[Thread, JdbcConnection]()

  def withConnection [ T ] ( f : JdbcConnection => T ) : T
    = {
      val thread = Thread.currentThread()
      openConnectionByThread.get(thread) match {
        case Some(cx) =>
          f(cx)
        case None =>
          val cx = fetchConnection()
          openConnectionByThread.update(thread, cx)
          try f(cx)
          finally {
            openConnectionByThread.remove(thread)
            returnConnection(cx)
          }
      }
    }


}
