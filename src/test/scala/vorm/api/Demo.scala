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
  populateDb()

  Db.query[Artist].all.map{_.names.values.head.head}.println()
  Db.query[Artist].all.map{_.id}.println()


}
