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

//  import SampleDb._
//
//  Db.query[Artist]
//    .filterEquals("names.value(1)", "Rolling Stones")
//    .fetchAll()
//    .map{_.names(Db.en).head}
//    .trace()

  case class EntityWithValuePropertyInOption
    ( a : Option[Int] )
  val db
    = new Instance( Entity[EntityWithValuePropertyInOption]() :: Nil,
                    "jdbc:h2:mem:test",
                    mode = Mode.Create
                    )
  db.save(EntityWithValuePropertyInOption(None))
  db.save(EntityWithValuePropertyInOption(Some(3)))

  db.fetchById[EntityWithValuePropertyInOption](1).trace()
  db.fetchById[EntityWithValuePropertyInOption](2).trace()

}
