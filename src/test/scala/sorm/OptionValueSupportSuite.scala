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
class OptionValueSupportSuite extends FunSuite with ShouldMatchers {

  import OptionValueSupportSuite._

  val db = TestingInstance.h2( Entity[EntityWithValuePropertyInOption]() ).connection()

  test("saving goes ok"){
    db.save(EntityWithValuePropertyInOption(None))
    db.save(EntityWithValuePropertyInOption(Some(3)))
    db.save(EntityWithValuePropertyInOption(Some(7)))
  }
  test("saved entities are correct"){
    db.fetchById[EntityWithValuePropertyInOption](1).a should be === None
    db.fetchById[EntityWithValuePropertyInOption](2).a should be === Some(3)
    db.fetchById[EntityWithValuePropertyInOption](3).a should be === Some(7)
  }
  test("equals filter"){
    db.access[EntityWithValuePropertyInOption]
      .whereEqual("a", None).fetchOne().get.id should be === 1
    db.access[EntityWithValuePropertyInOption]
      .whereEqual("a", Some(3)).fetchOne().get.id should be === 2
  }
  test("not equals filter"){
    db.access[EntityWithValuePropertyInOption]
      .whereNotEqual("a", None)
      .fetch().map{_.id.toInt}.toSet
      .should( not contain (1) and contain (3) and contain (2) )
    db.access[EntityWithValuePropertyInOption]
      .whereNotEqual("a", Some(3))
      .fetch().map{_.id.toInt}.toSet
      .should( not contain (2) and contain (1) and contain (3) )
  }

}
object OptionValueSupportSuite {

  case class EntityWithValuePropertyInOption
    ( a : Option[Int] )

}