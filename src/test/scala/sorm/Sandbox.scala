package sorm

import api._
import jdbc._
import persisted.Persisted
import reflection.Reflection
import samples._
import extensions.Extensions._

object Sandbox extends App {

  object ResponseType extends Enumeration {
    val Listing, Album = Value
  }


}
