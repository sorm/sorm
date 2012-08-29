package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


import samples.TestingInstance
import Sorm._

@RunWith(classOf[JUnitRunner])
class RangeSupportSuite extends FunSuite with ShouldMatchers {

  import RangeSupportSuite._

  val db
    = TestingInstance.h2( Entity[A]() )

  test("saving goes ok"){
    db.save(A( 2 to 4 ))
    db.save(A( 9 to 1 ))
  }
  test("saved entities are correct"){
    db.fetchById[A](1).get.a should equal (2 to 4)
    db.fetchById[A](2).get.a should equal (9 to 1)
  }
  test("equality filter"){
    db.all[A]
      .filterEqual("a", 2 to 4)
      .fetch()
      .map(_.id.toInt)
      .should(
        have size(1) and
        contain(1)
      )
  }

}
object RangeSupportSuite {

  case class A
    ( a : Range )

}