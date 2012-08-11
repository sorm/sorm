package vorm.api

import vorm._
import samples._
import extensions._

import com.codahale.logula.Logging
import org.apache.log4j.Level

object Demo extends App {

  Logging.configure { log =>
    log.level = Level.TRACE
    log.loggers("vorm.jdbc.ConnectionAdapter") = Level.TRACE
  }

  import SampleDb._

  Db.query[Artist]
    .filterEquals("styles.item.names.value.item", "Hard Rock")
    .fetchAll
    .trace()

}
