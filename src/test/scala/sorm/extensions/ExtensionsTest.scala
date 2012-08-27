package sorm.extensions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import sorm.extensions.Extensions._

@RunWith(classOf[JUnitRunner])
class ExtensionsTest extends FunSuite with ShouldMatchers {
  test("mapKeys") {
    Map("b" -> 1, "c" -> 4, "a" -> 9).mapKeys(_ + "1") should
    equal(Map("b1" -> 1, "c1" -> 4, "a1" -> 9))
  }
  test("asInstanceOfOption returns None for unmatching type"){
    8.toInstanceOf[String] should equal(None)
  }
  test("asInstanceOfOption returns Some for a matching type"){
    8.toInstanceOf[Int] should equal(Some(8))
  }
  test("asInstanceOfOption returns Some for an inheriting type"){
    List(1).toInstanceOf[Seq[Int]] should be ('defined)
  }
}