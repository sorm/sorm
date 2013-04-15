package sorm.test.types

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._

import samples._
import sorm.test.MultiInstanceSuite

@RunWith(classOf[JUnitRunner])
class SeqOfIntsSupportSuite extends FunSuite with ShouldMatchers with MultiInstanceSuite {
  import SeqOfIntsSupportSuite._

  def entities =  Set() + Entity[A]()
  instancesAndIds foreach { case (db, dbId) =>
    val a1 = db.save(A( Seq() ))
    val a2 = db.save(A( Seq(2, 9, 3) ))
    val a3 = db.save(A( Seq(4) ))
    val a4 = db.save(A( Seq() ))
    val a5 = db.save(A( Seq(3) ))


    test(dbId + " - Non matching equals query") {
      db.query[A].whereEqual("a", Seq(10)).fetch() should be ('empty)
    }
    test(dbId + " - Partially matching equals query") {
      db.query[A].whereEqual("a", Seq(2, 9)).fetch() should be ('empty)
      db.query[A].whereEqual("a", Seq(9)).fetch() should be ('empty)
      db.query[A].whereEqual("a", Seq(3)).fetch() should contain (a5)
      db.query[A].whereEqual("a", Seq(9, 3)).fetch() should be ('empty)
    }
    test(dbId + " - Empty seq equals query") {
      db.query[A].whereEqual("a", Seq()).fetch() should (contain(a1) and contain(a4))
    }
    test(dbId + " - Same seq equals query") {
      db.query[A].whereEqual("a", Seq(2, 9, 3)).fetch() should equal (Seq(a2))
    }
    test(dbId + " - Differently ordered seq") {
      db.query[A].whereEqual("a", Seq(9, 2, 3)).fetch() should be ('empty)
    }
    test(dbId + " - Equal on smaller size") {
      pending
    }
    test(dbId + " - Equal on bigger size") {
      pending
    }


    test(dbId + " - Not equals on seq of same size"){
      db.query[A].whereNotEqual("a", Seq(10)).fetch() should ( contain (a3) and contain (a5) )
      db.query[A].whereNotEqual("a", Seq(12,3,4)).fetch() should contain (a2)
    }
    test(dbId + " - Not equals on partially matching seq"){
      db.query[A].whereNotEqual("a", Seq(3)).fetch() should contain (a2)
      db.query[A].whereNotEqual("a", Seq(2, 9)).fetch() should contain (a2)
    }
    test(dbId + " - Not equals on totally matching seq"){
      db.query[A].whereNotEqual("a", Seq(2,9,3)).fetch() should not (contain(a2))
    }
    test(dbId + " - Not equals on empty seq"){
      db.query[A].whereNotEqual("a", Seq()).fetch()
        .should(
          contain (a2) and contain(a3) and contain(a5) and
          not contain(a1) and not contain(a4)
        )
    }
    test(dbId + " - Not equals on single item seq"){
      db.query[A].whereNotEqual("a", Seq(4)).fetch() should not (contain(a3))
    }
    test(dbId + " - Totally unmatching not equals query"){
      db.query[A].whereNotEqual("a", Seq(10)).fetch() should (
          contain (a1) and
          contain (a2) and
          contain (a3) and
          contain (a4)
        )
    }
  }

}
object SeqOfIntsSupportSuite {
  case class A ( a : Seq[Int] )
}