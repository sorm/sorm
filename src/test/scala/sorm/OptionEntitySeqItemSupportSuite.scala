package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import api._
import persisted._
import query._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import create._
import extensions.Extensions._

import samples._
import Sorm._

@RunWith(classOf[JUnitRunner])
class OptionEntitySeqItemSupportSuite extends FunSuite with ShouldMatchers {

  import OptionEntitySeqItemSupportSuite._

  val b1 = db.save(B("abc"))
  val b2 = db.save(B("cba"))

  test("saving goes ok"){
    db.save(A( Seq() ))
    db.save(A( Seq(Some(b1), None, Some(b2)) ))
    db.save(A( Seq(None, Some(b2)) ))
  }
  test("saved entities are correct"){
    db.fetchById[A](1).get.seq should be === Seq()
    db.fetchById[A](2).get.seq should be === Seq(Some(b1), None, Some(b2))
    db.fetchById[A](3).get.seq should be === Seq(None, Some(b2))
  }

}
object OptionEntitySeqItemSupportSuite {

  case class A
    ( seq : Seq[Option[B]] )
  case class B
    ( z : String )

  val db
    = new Instance( Entity[A]() :: Entity[B]() :: Nil,
                    "jdbc:h2:mem:test",
                    initialization = Initialization.DropAllCreate )

}