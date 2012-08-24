package sorm

import api._
import jdbc._
import mirrorQuirks.MirrorQuirks
import reflection._
import samples._
import extensions._

import com.codahale.logula.Logging
import org.apache.log4j.Level
import java.sql.DriverManager

object Sandbox extends App {

  Logging.configure { log =>
    log.level = Level.TRACE
  }

  object ResponseType extends Enumeration {
    val Listing, Album = Value
  }

  val t = typeTag[ResponseType.Value]
  t.tpe.asInstanceOf[{def pre: Any}].pre.trace()

//  ResponseType.Listing.reflected.reflection
//    .prettyString.trace()
//  val t = typeTag[ResponseType.Value]
//  t.prettyString.trace()
//  Reflection[ResponseType.Value]
//    .prettyString.trace()

}
