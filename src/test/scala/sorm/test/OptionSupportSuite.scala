package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import sext._, embrace._

@RunWith(classOf[JUnitRunner])
class OptionSupportSuite extends FunSuite with ShouldMatchers with MultiInstanceSuite {
  import OptionSupportSuite._

  def entities = Entity[EntityWithOptionInOption]() :: Nil

}
object OptionSupportSuite {

  case class EntityWithOptionInOption
    ( optionInOption : Option[Option[Int]] )

}

