package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import api._
import extensions.Extensions._

import samples._

@RunWith(classOf[JUnitRunner])
class EnumSupportSuite extends FunSuite with ShouldMatchers {
  import EnumSupportSuite._

  val db = TestingInstance.h2( Entity[A]() )

  val a1 = db.save(A(B.One))
  val a2 = db.save(A(B.Two))
  val a3 = db.save(A(B.Three))
  val a4 = db.save(A(B.Two))

  test(""){
    db.query[A].filterEquals("a", B.Two).fetchAll()
      .should(
        have size (2) and
        contain (a2)
      )
  }

}
object EnumSupportSuite {
  case class A ( a : B.Value )
  object B extends Enumeration {
    val One, Two, Three = Value
  }


}