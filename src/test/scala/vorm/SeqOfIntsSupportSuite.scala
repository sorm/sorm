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
class SeqOfIntsSupportSuite extends FunSuite with ShouldMatchers {

  import SeqOfIntsSupportSuite._
  import ArtistDb._

  val db = TestingInstance.h2( Entity[A]() )
  db.save(A( Seq() ))
  db.save(A( Seq(2, 9, 3) ))
  db.save(A( Seq(4) ))
  db.save(A( Seq() ))

  def fetchEqualingIds ( value : Seq[_] ) : Set[Long]
    = db.query[A].filterEquals("a", value).fetchAll().map{_.id}.toSet

  test("Non matching equals query") {
    fetchEqualingIds(Seq(10)) should be === Set()
  }
  test("Partially matching equals query") {
    fetchEqualingIds(Seq(2, 9)) should be === Set()
    fetchEqualingIds(Seq(9)) should be === Set()
    fetchEqualingIds(Seq(3)) should be === Set()
    fetchEqualingIds(Seq(9, 3)) should be === Set()
  }
  test("Empty seq equals query") {
    fetchEqualingIds(Seq()) should be === Set(1l, 4l)
  }
  test("Same seq equals query") {
    fetchEqualingIds(Seq(2, 9, 3)) should be === Set(2l)
  }
  test("Differently ordered seq") {
    fetchEqualingIds(Seq(9, 2, 3)) should be === Set()
  }

}
object SeqOfIntsSupportSuite {
  case class A ( a : Seq[Int] )
}