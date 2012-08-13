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

  case class EntityWithOptionInOption
    ( optionInOption : Option[Option[Int]] )

  test("Value property in Option")(pending)
  test("Option in Option fails on initialization"){
    evaluating {
        Entity[EntityWithOptionInOption]()
      } should produce [IllegalArgumentException]
  }
  test("Seq in Option")(pending)
  test("Map key in Option")(pending)
  test("Map value in Option")(pending)
  test("Seq item in Option")(pending)
  test("Set item in Option")(pending)

}

