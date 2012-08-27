package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Sorm._
import samples._

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
  test("self reference validation"){
    pending
  }
}
object SormValidationSuite {
  case class A
    ( a : Seq[Option[(B, Int)]] )
  case class B
    ( a : Int )
}