package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import api._
import samples._

@RunWith(classOf[JUnitRunner])
class SormValidationSuite extends FunSuite with ShouldMatchers {
  import SormValidationSuite._

  test("`Any` type is not supported"){
    evaluating {
      new Instance(Entity[D]() :: Nil, "jdbc:h2:mem:test")
    } should produce [Instance#ValidationException]
  }
  test("referred entities validation"){
    evaluating {
      new Instance(
        Entity[A]() :: Nil,
        "jdbc:h2:mem:test"
      )
    } should produce [Instance#ValidationException]
  }
  test("Correct instantiation doesnt throw exceptions"){
    new Instance(
      Entity[A]() :: Entity[B]() :: Entity[C]() :: Nil,
      "jdbc:h2:mem:test"
    ) should not equal (false)
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
  case class D
    ( a : Seq[Any] )
}