package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._

import samples._

@RunWith(classOf[JUnitRunner])
class SeqOfEnumsSupportSuite extends FunSuite with ShouldMatchers {
  import SeqOfEnumsSupportSuite._

  TestingInstances.instances( Set() + Entity[A]() ) foreach { case (db, dbId) =>

    val a1 = db.save(A(B.Two :: Nil))
    val a2 = db.save(A(B.Two :: B.Two :: B.One :: Nil))
    val a3 = db.save(A(B.Three :: Nil))
    val a4 = db.save(A(B.Two :: Nil))
    val a5 = db.save(A(Nil))

    test(dbId + " - Equal query"){
      db.access[A].whereEqual("a", Seq(B.Two)).fetch()
        .should(
          have size (2) and
          contain (a4)
        )
    }
    test(dbId + " - Equal empty Seq query"){
      db.access[A].whereEqual("a", Seq()).fetch()
        .should(
          contain (a5) and not contain (a4)
        )
    }
    test(dbId + " - Not equals empty Seq query"){
      db.access[A].whereNotEqual("a", Seq()).fetch()
        .should(
          not be 'empty and
          not contain (a5)
        )
    }
    test(dbId + " - Not equals query"){
      db.access[A].whereNotEqual("a", Seq(B.Three)).fetch()
        .should(
          not be 'empty and
          not contain (a3)
        )
    }
    test(dbId + " - Equal inexistent Seq query"){
      db.access[A].whereEqual("a", Seq(B.Three, B.Three)).fetch()
        .should(
          be ('empty)
        )
    }
    test(dbId + " - Not equals inexistent Seq query"){
      db.access[A].whereNotEqual("a", Seq(B.Three, B.Three)).fetch()
        .should(
          not be ('empty)
        )
    }
  }
}
object SeqOfEnumsSupportSuite {

  case class A ( a : Seq[B.Value] )
  object B extends Enumeration {
    val One, Two, Three = Value
  }


}