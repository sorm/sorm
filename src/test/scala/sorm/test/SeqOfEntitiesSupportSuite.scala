package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._

@RunWith(classOf[JUnitRunner])
class SeqOfEntitiesSupportSuite extends FunSuite with ShouldMatchers {
  import SeqOfEntitiesSupportSuite._

  TestingInstances.instances( Set() + Entity[A]() + Entity[B]() ) foreach { case (db, dbId) =>
    val b1 = db.save(B(23))
    val b2 = db.save(B(0))
    val b3 = db.save(B(0))
    val b4 = db.save(B(12))
    val b5 = db.save(B(12))

    val a1 = db.save(A( Seq() ))
    val a2 = db.save(A( Seq(b1, b2, b3) ))
    val a3 = db.save(A( Seq() ))
    val a4 = db.save(A( Seq(b4) ))

    test(dbId + " - Non matching equals query") {
      db.query[A].whereEqual("a", Seq(b5)).fetch() should be ('empty)
      db.query[A].whereEqual("a", Seq(b1, b2, b4)).fetch() should be ('empty)
    }
    test(dbId + " - Partially matching equals query") {
      db.query[A].whereEqual("a", Seq(b2)).fetch() should be ('empty)
      db.query[A].whereEqual("a", Seq(b1, b2)).fetch() should be ('empty)
      db.query[A].whereEqual("a", Seq(b3)).fetch() should be ('empty)
      db.query[A].whereEqual("a", Seq(b2, b3)).fetch() should be ('empty)
    }
    test(dbId + " - Empty seq equals query") {
      db.query[A].whereEqual("a", Seq()).fetch().toSet should be (Set(a1, a3))
    }
    test(dbId + " - Same seq equals query") {
      db.query[A].whereEqual("a", Seq(b1, b2, b3)).fetch() should (contain(a2) and have length(1))
      db.query[A].whereEqual("a", Seq(b4)).fetch() should (contain(a4) and have length(1))
    }
    test(dbId + " - Differently ordered seq") {
      db.query[A].whereEqual("a", Seq(b1, b3, b2)).fetch() should be ('empty)
      db.query[A].whereEqual("a", Seq(b2, b3, b1)).fetch() should be ('empty)
    }
    test(dbId + " - Equal on empty seq does not include non empty seqs") {
      db.query[A].whereEqual("a", Seq()).fetch()
        .should( not contain (a2) and not contain (a4) )
    }

    test(dbId + " - Everything matches not equals on inexistent") {
      db.query[A].whereNotEqual("a", Seq(b5)).fetch()
        .should( contain(a1) and contain(a2) and contain(a3) and contain(a4) )
    }
    test(dbId + " - A partially matching item does not get excluded from results on not equals"){
      db.query[A]
        .whereNotEqual("a", Seq(b1, b3))
        .fetch()
        .should( contain (a2) )
    }
    test(dbId + " - Not equals on empty seq does not return empty seqs") {
      db.query[A]
        .whereNotEqual("a", Seq())
        .fetch()
        .should( not contain (a1) and not contain (a3) )
    }

  }

}
object SeqOfEntitiesSupportSuite {
  case class A ( a : Seq[B] )
  case class B ( a : Int )
}