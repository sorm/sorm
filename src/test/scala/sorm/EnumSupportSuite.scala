package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import api._
import extensions.Extensions._
import Sorm._

import samples._

@RunWith(classOf[JUnitRunner])
class EnumSupportSuite extends FunSuite with ShouldMatchers {
  import EnumSupportSuite._

  val db = TestingInstance.mysql( Entity[A]() )

  val a1 = db.save(A(B.One))
  val a2 = db.save(A(B.Two))
  val a3 = db.save(A(B.Three))
  val a4 = db.save(A(B.Two))

  test("Equality query"){
    db.query[A].filterEqual("a", B.Two).fetchAll()
      .should(
        have size (2) and
        contain (a2)
      )
  }
  test("Not equals query"){
    db.query[A].filterNotEqual("a", B.Two).fetchAll()
      .should(
        not be 'empty and
        contain (a1) and
        contain (a3)
      )
  }

}
object EnumSupportSuite {
  case class A ( a : B.Value )
  object B extends Enumeration {
    val One, Two, Three = Value
  }


}