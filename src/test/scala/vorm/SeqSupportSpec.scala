package vorm

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
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
class SeqSupportSpec extends FunSpec with ShouldMatchers {
  import SeqSupportSpec._

  // describe("Ids returned by 'equals' query on Seq[Int]") {
  describe("An `equals` query on Seq[Int]") {
    val db = instance( Entity[A]() )
    db.save(A( Seq() ))
    db.save(A( Seq(2, 9, 3) ))
    db.save(A( Seq(4) ))
    db.save(A( Seq() ))

    def fetchIds ( value : Seq[_] ) : Set[Long]
      = db.query[A].filterEquals("a", value).fetchAll().map{_.id}.toSet

    it("should be empty on non matching value") {
      fetchIds(Seq(10)) === Set()
    }
    it("should be empty on partially matching value") {
      fetchIds(Seq(2, 9)) === Set()
      fetchIds(Seq(9)) === Set()
      fetchIds(Seq(3)) === Set()
      fetchIds(Seq(9, 3)) === Set()
    }
    it("on empty Seq should match it only") {
      fetchIds(Seq()) === Set(1l, 4l)
    }
    it("should match the same Seq") {
      fetchIds(Seq(2, 9, 3)) === Set(2l)
    }
    it("should not match a differently ordered Seq") {
      fetchIds(Seq(9, 2, 3)) === Set()
    }
  }

}
object SeqSupportSpec {
  def instance ( entities : Entity[_]* )
    = new Instance( entities, "jdbc:h2:mem:test", mode = Mode.DropAllCreate )

  case class A ( a : Seq[Int] )

}