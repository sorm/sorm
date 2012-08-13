package vorm

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
    .filterEquals("names.value(1)", "Rolling Stones")
    .fetchAll()
    .map{_.names(Db.en).head}
    .trace()

}
