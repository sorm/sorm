package sorm.test

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

@RunWith(classOf[JUnitRunner])
class OptionTupleSupportSuite extends FunSuite with ShouldMatchers {

  import OptionTupleSupportSuite._

  val db = TestingInstance.mysql(Entity[A]()).connection()

  db.save(A( None ))
  db.save(A( Some(2 -> None) ))
  db.save(A( Some(56 -> Some("asdf")) ))

  test("top none"){
    db.fetchById[A](1).a should be === None
  }
  test("deep none"){
    db.fetchById[A](2).a should be === Some(2 -> None)
  }
  test("deep some"){
    db.fetchById[A](3).a should be === Some(56 -> Some("asdf"))
  }

}
object OptionTupleSupportSuite {

  case class A
    ( a : Option[(Int, Option[String])] )

}