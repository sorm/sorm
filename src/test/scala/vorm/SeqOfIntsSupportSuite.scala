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
import com.codahale.logula.Logging
import org.apache.log4j.Level

@RunWith(classOf[JUnitRunner])
class SeqOfIntsSupportSuite extends FunSuite with ShouldMatchers {

  import SeqOfIntsSupportSuite._

  Logging.configure { log =>
    log.level = Level.TRACE
    log.loggers("vorm.jdbc.ConnectionAdapter") = Level.TRACE
  }

  test("Non matching equals query") {
    fetchEqualingIds(Seq(10)) should be === Set()
  }
  test("Partially matching equals query") {
    fetchEqualingIds(Seq(2, 9)) should be === Set()
    fetchEqualingIds(Seq(9)) should be === Set()
    fetchEqualingIds(Seq(3)) should be === Set(5l)
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

  test("Totally unmatching not equals query"){
    fetchNotEqualingIds(Seq(10)) should (
        contain (1l) and
        contain (2l) and
        contain (3l) and
        contain (4l)
      )
  }
  test("Partially unmatching not equals query"){
    fetchNotEqualingIds(Seq(3)) should contain (2l)
    fetchNotEqualingIds(Seq(2, 9)) should contain (2l)
  }
  test("Completely unmatching not equals query"){
    fetchNotEqualingIds(Seq(2,9,3)) should not contain (2l)
  }
  test("Empty seq not equals query"){
    fetchNotEqualingIds(Seq())
      .should(
        contain (2l) and contain(3l) and contain(5l) and
        not contain(1l) and not contain(4l)
      )
  }
  test("Single item not equals query"){
    fetchNotEqualingIds(Seq(4)) should(
        not contain(3l)
      )
  }


}
object SeqOfIntsSupportSuite {
  case class A ( a : Seq[Int] )

  val db = TestingInstance.h2( Entity[A]() )
  db.save(A( Seq() ))
  db.save(A( Seq(2, 9, 3) ))
  db.save(A( Seq(4) ))
  db.save(A( Seq() ))
  db.save(A( Seq(3) ))

  def fetchEqualingIds ( value : Seq[_] ) : Set[Long]
    = db.query[A].filterEquals("a", value).fetchAll().map{_.id}.toSet
  def fetchNotEqualingIds ( value : Seq[_] ) : Set[Long]
    = db.query[A].filterNotEquals("a", value).fetchAll().map{_.id}.toSet
}