package vorm

import api._
import samples._
import extensions._

import com.codahale.logula.Logging
import org.apache.log4j.Level

object Demo extends App {

  Logging.configure { log =>
    log.level = Level.TRACE
    log.loggers("vorm.jdbc.ConnectionAdapter") = Level.TRACE
  }


  case class A
    ( seq : Seq[Option[B]] )
  case class B
    ( z : String )

  val db
    = new Instance( Entity[A]() :: Entity[B]() :: Nil,
                    "jdbc:h2:mem:test",
                    mode = Mode.DropAllCreate )

  val b1 = db.save(B("abc"))
  val b2 = db.save(B("cba"))

  db.save(A( Seq() ))
  db.save(A( Seq(Some(b1), None, Some(b2)) ))
  db.save(A( Seq(None, Some(b2)) ))

  db.fetchById[A](1).get.seq.trace()
  db.fetchById[A](2).get.seq.trace()
  db.fetchById[A](3).get.seq.trace()

}
