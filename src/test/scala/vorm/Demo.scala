package vorm

import api._
import jdbc._
import samples._
import extensions._

import com.codahale.logula.Logging
import org.apache.log4j.Level
import java.sql.DriverManager

object Demo extends App {

  Logging.configure { log =>
    log.level = Level.TRACE
    log.loggers("vorm.jdbc.ConnectionAdapter") = Level.TRACE
  }

  new ConnectionAdapter(DriverManager.getConnection("jdbc:h2:mem:test"))
    .executeQuery( Statement("SELECT NOW()") )
    .parseAndClose()
    .head.head
    .trace()
}
