package vorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vorm._
import api._
import persisted._
import query._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import create._
import extensions._

import samples._

@RunWith(classOf[JUnitRunner])
class SeqOfEntitiesSupportSuite extends FunSuite with ShouldMatchers {

  import SeqOfEntitiesSupportSuite._

  test("Non matching equals query") {
    fetchEqualingIds( Seq(b5) ) === Set()
    fetchEqualingIds( Seq(b1, b2, b4) ) === Set(2l)
  }
  test("Partially matching equals query") {
    fetchEqualingIds( Seq(b2) ) === Set()
    fetchEqualingIds( Seq(b1, b2) ) === Set()
    fetchEqualingIds( Seq(b3) ) === Set()
    fetchEqualingIds( Seq(b2, b3) ) === Set()
  }
  test("Empty seq equals query") {
    fetchEqualingIds( Seq() ) === Set(1l, 3l)
  }
  test("Same seq equals query") {
    fetchEqualingIds( Seq(b1, b2, b3) ) === Set(2l)
    fetchEqualingIds( Seq(b4) ) === Set(4l)
  }
  test("Differently ordered seq") {
    fetchEqualingIds( Seq(b1, b3, b2) ) === Set()
    fetchEqualingIds( Seq(b2, b3, b1) ) === Set()
  }

}
object SeqOfEntitiesSupportSuite {
  case class A ( a : Seq[B] )
  case class B ( a : Int )

  val db = TestingInstance.h2( Entity[A](), Entity[B]() )

  val b1 = db.save(B(23))
  val b2 = db.save(B(0))
  val b3 = db.save(B(0))
  val b4 = db.save(B(12))
  val b5 = db.save(B(12))

  db.save(A( Seq() ))
  db.save(A( Seq(b1, b2, b3) ))
  db.save(A( Seq() ))
  db.save(A( Seq(b4) ))

  def fetchEqualingIds ( value : Seq[_] ) : Set[Long]
    = db.query[A].filterEquals("a", value).fetchAll().map{_.id}.toSet

}