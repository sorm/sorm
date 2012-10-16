package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._

import samples._
import Sorm._

@RunWith(classOf[JUnitRunner])
class InFilterTest extends FunSuite with ShouldMatchers {
  import InFilterTest._

  val db = TestingInstance.h2( Entity[A]() )

  val a1 = db.save(A(1))
  val a2 = db.save(A(2))
  val a3 = db.save(A(3))


  test("empty value"){
    db.access[A].whereIn("a", Seq()).fetchOne().should(equal(None))
  }
  test("valid value"){
    db.access[A].whereIn("a", Seq(2)).fetchOne().should(equal(Some(a2)))
  }

}
object InFilterTest {

  case class A ( a : Int )

}