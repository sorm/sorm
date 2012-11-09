package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._

import samples._

@RunWith(classOf[JUnitRunner])
class SeqOfIntsSupportSuite extends FunSuite with ShouldMatchers {
  import SeqOfIntsSupportSuite._

  TestingInstances.instances( Set() + Entity[A]() ) foreach { case (db, dbId) =>
    db.save(A( Seq() ))
    db.save(A( Seq(2, 9, 3) ))
    db.save(A( Seq(4) ))
    db.save(A( Seq() ))
    db.save(A( Seq(3) ))

    def fetchEqualingIds ( value : Seq[_] ) : Set[Long]
      = db.query[A].whereEqual("a", value).fetch().map{_.id}.toSet
    def fetchNotEqualingIds ( value : Seq[_] ) : Set[Long]
      = db.query[A].whereNotEqual("a", value).fetch().map{_.id}.toSet

    test(dbId + " - Non matching equals query") {
      fetchEqualingIds(Seq(10)) should be === Set()
    }
    test(dbId + " - Partially matching equals query") {
      fetchEqualingIds(Seq(2, 9)) should be === Set()
      fetchEqualingIds(Seq(9)) should be === Set()
      fetchEqualingIds(Seq(3)) should be === Set(5l)
      fetchEqualingIds(Seq(9, 3)) should be === Set()
    }
    test(dbId + " - Empty seq equals query") {
      fetchEqualingIds(Seq()) should be === Set(1l, 4l)
    }
    test(dbId + " - Same seq equals query") {
      fetchEqualingIds(Seq(2, 9, 3)) should be === Set(2l)
    }
    test(dbId + " - Differently ordered seq") {
      fetchEqualingIds(Seq(9, 2, 3)) should be === Set()
    }
    test(dbId + " - Equal on smaller size") {
      pending
    }
    test(dbId + " - Equal on bigger size") {
      pending
    }


    test(dbId + " - Not equals on seq of same size"){
      fetchNotEqualingIds(Seq(10)) should ( contain (3l) and contain (5l) )
      fetchNotEqualingIds(Seq(12,3,4)) should contain (2l)
    }
    test(dbId + " - Not equals on partially matching seq"){
      fetchNotEqualingIds(Seq(3)) should contain (2l)
      fetchNotEqualingIds(Seq(2, 9)) should contain (2l)
    }
    test(dbId + " - Not equals on totally matching seq"){
      fetchNotEqualingIds(Seq(2,9,3)) should not contain (2l)
    }
    test(dbId + " - Not equals on empty seq"){
      fetchNotEqualingIds(Seq())
        .should(
          contain (2l) and contain(3l) and contain(5l) and
          not contain(1l) and not contain(4l)
        )
    }
    test(dbId + " - Not equals on single item seq"){
      fetchNotEqualingIds(Seq(4)) should not contain(3l)
    }
    test(dbId + " - Totally unmatching not equals query"){
      fetchNotEqualingIds(Seq(10)) should (
          contain (1l) and
          contain (2l) and
          contain (3l) and
          contain (4l)
        )
    }
  }

}
object SeqOfIntsSupportSuite {
  case class A ( a : Seq[Int] )
}