package sorm

import api._
import jdbc._
import persisted.Persisted
import reflection.Reflection
import samples._
import extensions.Extensions._

import java.sql.DriverManager

object Sandbox extends App {

  case class A ( a : Int )

  class B (val b : Int) extends A (2) {
    override def equals
      ( other : Any )
      = other match {
          case other : B => b == other.b
        }
  }

  (
    Persisted(A(1), 23) == Persisted(A(1), 12)
  ).prettyString.trace()

   Reflection[A].signature.prettyString.trace()

}
