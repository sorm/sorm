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

  test("equals filter"){
    val db = instance( Entity[A]() )
    db.save(A( Seq() ))
    db.save(A( Seq(2, 9, 3) ))
    db.save(A( Seq(4) ))

    db.query[A]
      .filterEquals("a", Seq(9)).fetchAll() === Nil
    db.query[A]
      .filterEquals("a", Seq()).fetchAll()
      .map{_.id}.toSet === Set(1l)
    db.query[A]
      .filterEquals("a", Seq(2, 9, 3)).fetchAll()
      .map{_.id}.toSet === Set(2l)
    db.query[A]
      .filterEquals("a", Seq(2, 9)).fetchAll()
      .map{_.id}.toSet === Set()
    db.query[A].filterEquals("a", Seq(9)).fetchAll()
      .map{_.id}.toSet === Set(3l)
  }

}
object SeqOfIntsSupportSuite {
  def instance ( entities : Entity[_]* )
    = new Instance( entities, "jdbc:h2:mem:test", mode = Mode.DropAllCreate )
  case class A ( a : Seq[Int] )

}