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
class OptionValueSupportSuite extends FunSuite with ShouldMatchers {

  import OptionValueSupportSuite._

  test("saving goes ok"){
    db.save(EntityWithValuePropertyInOption(None))
    db.save(EntityWithValuePropertyInOption(Some(3)))
    db.save(EntityWithValuePropertyInOption(Some(7)))
  }
  test("saved entities are correct"){
    db.fetchById[EntityWithValuePropertyInOption](1).get.a should be === None
    db.fetchById[EntityWithValuePropertyInOption](2).get.a should be === Some(3)
    db.fetchById[EntityWithValuePropertyInOption](3).get.a should be === Some(7)
  }
  test("equals filter"){
    db.query[EntityWithValuePropertyInOption]
      .filterEquals("a", None).fetchOne().get.id should be === 1
    db.query[EntityWithValuePropertyInOption]
      .filterEquals("a", Some(3)).fetchOne().get.id should be === 2
  }
  test("not equals filter"){
    db.query[EntityWithValuePropertyInOption]
      .filterNotEquals("a", None).fetchOne().get.id should be === 2
    db.query[EntityWithValuePropertyInOption]
      .filterNotEquals("a", Some(3)).fetchOne().get.id should be === 1
  }

}
object OptionValueSupportSuite {

  case class EntityWithValuePropertyInOption
    ( a : Option[Int] )

  val db
    = new Instance( Entity[EntityWithValuePropertyInOption]() :: Nil,
                    "jdbc:h2:mem:test",
                    mode = Mode.DropAllCreate )

}