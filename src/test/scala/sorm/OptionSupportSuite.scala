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
import mappings._
import jdbc._
import sext._, embrace._

import samples._
import Sorm._

@RunWith(classOf[JUnitRunner])
class OptionSupportSuite extends FunSuite with ShouldMatchers {
  import OptionSupportSuite._


}
object OptionSupportSuite {

  case class EntityWithOptionInOption
    ( optionInOption : Option[Option[Int]] )

}

