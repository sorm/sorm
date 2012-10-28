package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import core._
import sext._, embrace._
import api._

import samples._

@RunWith(classOf[JUnitRunner])
class EnumSupportSuite extends FunSuite with ShouldMatchers {
  import EnumSupportSuite._

  val db = TestingInstance.mysql( Entity[A]() ).connection()

  val a1 = db.save(A(B.One))
  val a2 = db.save(A(B.Two))
  val a3 = db.save(A(B.Three))
  val a4 = db.save(A(B.Two))

  test("Equality query"){
    db.access[A].whereEqual("a", B.Two).fetch()
      .should(
        have size (2) and
        contain (a2)
      )
  }
  test("Not equals query"){
    db.access[A].whereNotEqual("a", B.Two).fetch()
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