package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import core._
import persisted._
import query._
import reflection._
import mappings._
import jdbc._
import sext._

import samples._
import Sorm._

@RunWith(classOf[JUnitRunner])
class OptionEntitySeqItemSupportSuite extends FunSuite with ShouldMatchers {

  import OptionEntitySeqItemSupportSuite._

  val db = TestingInstance.mysql(Entity[A](), Entity[B]())

  val b1 = db.save(B("abc"))
  val b2 = db.save(B("cba"))

  test("saving goes ok"){
    db.save(A( Seq() ))
    db.save(A( Seq(Some(b1), None, Some(b2)) ))
    db.save(A( Seq(None, Some(b2)) ))
    db.save(A( Seq(None) ))
  }
  test("empty seq"){
    db.fetchById[A](1).get.seq should be === Seq()
  }
  test("seq of none"){
    db.fetchById[A](4).get.seq should be === Seq(None)
  }
  test("not empty seqs are correct"){
    db.fetchById[A](2).get.seq should be === Seq(Some(b1), None, Some(b2))
    db.fetchById[A](3).get.seq should be === Seq(None, Some(b2))
  }

}
object OptionEntitySeqItemSupportSuite {

  case class A
    ( seq : Seq[Option[B]] )
  case class B
    ( z : String )
}