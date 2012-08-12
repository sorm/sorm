package vorm.api

import vorm._
import reflection.Reflection
import samples._
import extensions._
import query.Query._

import com.codahale.logula.Logging
import org.apache.log4j.Level
import structure.mapping.{EntityMapping, Mapping}

object Demo extends App {

  Logging.configure { log =>
    log.level = Level.TRACE
    log.loggers("vorm.jdbc.ConnectionAdapter") = Level.TRACE
  }

  import SampleDb._

  Db.query[Artist]
    .filterEquals("names.value(1)", "Rolling Stones")
    .fetchAll()
    .trace()

}
