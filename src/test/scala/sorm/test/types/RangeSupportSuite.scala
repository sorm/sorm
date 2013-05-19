package sorm.test.types

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import sorm.test.MultiInstanceSuite

@RunWith(classOf[JUnitRunner])
class RangeSupportSuite extends FunSuite with ShouldMatchers with MultiInstanceSuite {

  import RangeSupportSuite._

  def entities =  Set() + Entity[A]()
  instancesAndIds foreach { case (db, dbId) =>

    val a1 = db.save(A( 2 to 4 ))
    val a2 = db.save(A( 9 to 1 ))

    test(dbId + " - saved entities are correct"){
      db.fetchById[A](a1.id).a should equal (2 to 4)
      db.fetchById[A](a2.id).a should equal (9 to 1)
    }
    test(dbId + " - equality filter"){
      db.query[A]
        .whereEqual("a", 2 to 4)
        .fetch()
        .map(_.id)
        .should(
          have size(1) and
          contain(a1.id)
        )
    }
    test(dbId + " - 0 to 0 range"){
      val a = db.save(A(0 to 0))
      db.fetchById[A](a.id).a should equal (0 to 0)
    }

  }
}
object RangeSupportSuite {

  case class A
    ( a : Range )

}