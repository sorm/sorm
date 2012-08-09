package vorm.api

import vorm._
import samples._
import extensions._

import com.codahale.logula.Logging
import org.apache.log4j.Level

object Demo extends App {

  Logging.configure { log =>
    log.level = Level.TRACE
    log.loggers("vorm.jdbc.ConnectionAdapter") = Level.WARN
  }

  import SampleDb._

  Db.query[Artist].fetchAll().map{_.names.values.head.head}.trace()
  Db.query[Artist].fetchAll().map{_.id}.trace()
  Db.query[Artist].fetchOne().trace()

}
