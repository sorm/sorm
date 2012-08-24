package sorm.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import samples._

@RunWith(classOf[JUnitRunner])
class InstanceSuite extends FunSuite with ShouldMatchers {
  import InstanceSuite._

  test("referred entities validation"){
    evaluating {
      new Instance(
        Entity[A]() :: Nil,
        "jdbc:h2:mem:test"
      )
    } should produce [IllegalArgumentException]
  }
  test("self reference validation"){
    pending
  }
}
object InstanceSuite {
  case class A
    ( a : Seq[Option[(B, Int)]] )
  case class B
    ( a : Int )
}