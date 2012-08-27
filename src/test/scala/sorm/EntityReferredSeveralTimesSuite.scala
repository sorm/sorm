package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import core._
import extensions.Extensions._
import Sorm._

import samples._

@RunWith(classOf[JUnitRunner])
class EntityReferredSeveralTimesSuite extends FunSuite with ShouldMatchers {
  import EntityReferredSeveralTimesSuite._

  val db = TestingInstance.h2( Entity[A](), Entity[B]() )

  val b1 = db.save(B(23))
  val b2 = db.save(B(0))
  val b3 = db.save(B(0))
  val b4 = db.save(B(12))
  val b5 = db.save(B(12))

  val a1 = db.save(A(b1, Seq(b4, b1)))
  val a2 = db.save(A(b2, Seq(b2)))
  val a3 = db.save(A(b3, Seq(b2)))
  val a4 = db.save(A(b4, Seq(b1)))

  test("Matches on other properties must not be included") {
    db.query[A].filterEqual("b", b2).fetchAll()
      .should( not contain (a3) and contain (a2) )
  }
  test("Matches on other properties must not be included 2") {
    db.query[A].filterEqual("bs.item", b4).fetchAll()
      .should( not contain (a4) )
  }


}
object EntityReferredSeveralTimesSuite {
  case class A (b : B, bs : Seq[B])
  case class B (x : Int)
}