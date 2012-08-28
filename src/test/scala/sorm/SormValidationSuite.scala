package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Sorm._
import samples._
import sorm.Sorm.ValidationException

@RunWith(classOf[JUnitRunner])
class SormValidationSuite extends FunSuite with ShouldMatchers {
  import SormValidationSuite._

  test("referred entities validation"){
    evaluating {
      new Instance(
        Entity[A]() :: Nil,
        "jdbc:h2:mem:test"
      )
    } should produce [ValidationException]
  }
  test("Correct instantiation doesn't throw exceptions"){
    new Instance(
      Entity[A]() :: Entity[B]() :: Entity[C]() :: List.empty[Entity[_]],
      "jdbc:h2:mem:test"
    )
  }
  test("self reference validation"){
    pending
  }
}
object SormValidationSuite {
  case class A
    ( a : Seq[Option[(B, Int)]], b : B, c : Seq[C] )
  case class B
    ( a : Int, b : C )
  case class C
    ( a : Int )
}