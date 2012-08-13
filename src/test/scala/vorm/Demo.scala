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
    ( a : Option[(Int, Option[String])] )

  val db
    = new Instance( Entity[A]() :: Nil,
                    "jdbc:h2:mem:test",
                    mode = Mode.DropAllCreate )

  db.save(A( None ))
  db.save(A( Some(2 -> None) ))
  db.save(A( Some(56 -> Some("asdf")) ))

  db.fetchById[A](1).get.a.trace()
  db.fetchById[A](2).get.a.trace()
  db.fetchById[A](3).get.a.trace()

}
