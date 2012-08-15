package vorm

import api._
import jdbc._
import samples._
import extensions._

import com.codahale.logula.Logging
import org.apache.log4j.Level
import java.sql.DriverManager

object Sandbox extends App {

  Logging.configure { log =>
    log.level = Level.TRACE
    log.loggers("vorm.jdbc.ConnectionAdapter") = Level.TRACE
  }

  case class A ( a : Seq[Int] )

  val db = TestInstance.hsql( Entity[A]() )
  db.save(A( Seq() ))
  db.save(A( Seq(2, 9, 3) ))
  db.save(A( Seq(4) ))
  db.save(A( Seq() ))

  def fetchIds ( value : Seq[_] ) : Set[Long]
    = db.query[A].filterEquals("a", value).fetchAll().map{_.id}.toSet

  fetchIds(Seq(2, 9, 3)).trace()

}
