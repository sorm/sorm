package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import core._
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
class OptionSupportSuite extends FunSuite with ShouldMatchers {
  import OptionSupportSuite._

  test("Option in Option fails on initialization"){
    evaluating {
      Entity[EntityWithOptionInOption]()
    } should produce [ValidationException]
  }

}
object OptionSupportSuite {

  case class EntityWithOptionInOption
    ( optionInOption : Option[Option[Int]] )

}

