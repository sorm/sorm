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

  //  equals
  test("Non matching equals query") {
    fetchEqualingIds( Seq(b5) ) should be === Set()
    fetchEqualingIds( Seq(b1, b2, b4) ) should be === Set()
  }
  test("Partially matching equals query") {
    fetchEqualingIds( Seq(b2) ) should be === Set()
    fetchEqualingIds( Seq(b1, b2) ) should be === Set()
    fetchEqualingIds( Seq(b3) ) should be === Set()
    fetchEqualingIds( Seq(b2, b3) ) should be === Set()
  }
  test("Empty seq equals query") {
    fetchEqualingIds( Seq() ) should be === Set(1l, 3l)
  }
  test("Same seq equals query") {
    fetchEqualingIds( Seq(b1, b2, b3) ) should be === Set(2l)
    fetchEqualingIds( Seq(b4) ) should be === Set(4l)
  }
  test("Differently ordered seq") {
    fetchEqualingIds( Seq(b1, b3, b2) ) should be === Set()
    fetchEqualingIds( Seq(b2, b3, b1) ) should be === Set()
  }

  //  not equals
  test("Not equals query") {
    db.query[A]
      .filterNotEquals("a", Seq(b5))
      .fetchAll().map(_.id.toInt).toSet
      .should( not contain (5) )
    db.query[A]
      .filterNotEquals("a", Seq(b3, b1))
      .fetchAll().map(_.id.toInt).toSet
      .should( contain (1) and contain (2) )
    db.query[A]
      .filterNotEquals("a", Seq())
      .fetchAll().map(_.id.toInt).toSet
      .should( not contain (2) and not contain (4) and contain (1) )
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
  def fetchNotEqualingIds ( value : Seq[_] ) : Set[Long]
    = db.query[A].filterNotEquals("a", value).fetchAll().map{_.id}.toSet

}