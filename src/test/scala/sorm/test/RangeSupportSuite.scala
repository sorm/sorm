package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import samples.TestingInstance
import api._

@RunWith(classOf[JUnitRunner])
class RangeSupportSuite extends FunSuite with ShouldMatchers {

  import RangeSupportSuite._

  val db = TestingInstance.h2( Entity[A]() ).connection()

  test("saving goes ok"){
    db.save(A( 2 to 4 ))
    db.save(A( 9 to 1 ))
  }
  test("saved entities are correct"){
    db.fetchById[A](1).a should equal (2 to 4)
    db.fetchById[A](2).a should equal (9 to 1)
  }
  test("equality filter"){
    db.access[A]
      .whereEqual("a", 2 to 4)
      .fetch()
      .map(_.id.toInt)
      .should(
        have size(1) and
        contain(1)
      )
  }
  test("0 to 0 range"){
    val a = db.save(A(0 to 0))
    db.fetchById[A](a.id).a should equal (0 to 0)
  }

}
object RangeSupportSuite {

  case class A
    ( a : Range )

}