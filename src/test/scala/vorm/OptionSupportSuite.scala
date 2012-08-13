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
class OptionSupportSuite extends FunSuite with ShouldMatchers {
  import OptionSupportSuite._

  test("Option in Option fails on initialization"){
    evaluating {
        Entity[EntityWithOptionInOption]()
      } should produce [IllegalArgumentException]
  }

  test("Value property in Option"){
    val db
      = new Instance( Entity[EntityWithValuePropertyInOption]() :: Nil,
                      "jdbc:h2:mem:test",
                      mode = Mode.DropAllCreate
                      )
    db.save(EntityWithValuePropertyInOption(None))
    db.save(EntityWithValuePropertyInOption(Some(3)))
    db.save(EntityWithValuePropertyInOption(Some(7)))

    db.fetchById[EntityWithValuePropertyInOption](1).get.a === None
    db.fetchById[EntityWithValuePropertyInOption](2).get.a === Some(3)

    db.query[EntityWithValuePropertyInOption]
      .filterEquals("a", None).fetchOne().get.id === 1
    db.query[EntityWithValuePropertyInOption]
      .filterEquals("a", Some(3)).fetchOne().get.id === 2
  }

  test("Seq in Option")(pending)
  test("Map key in Option")(pending)
  test("Map value in Option")(pending)
  test("Seq item in Option")(pending)
  test("Set item in Option")(pending)

}
object OptionSupportSuite {

  case class EntityWithOptionInOption
    ( optionInOption : Option[Option[Int]] )
  case class EntityWithValuePropertyInOption
    ( a : Option[Int] )
}

