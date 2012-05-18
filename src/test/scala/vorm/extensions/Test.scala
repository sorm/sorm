package vorm.extensions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite with ShouldMatchers {
  test("mapKeys") {
    Map("b" -> 1, "c" -> 4, "a" -> 9).mapKeys(_ + "1") should
    equal(Map("b1" -> 1, "c1" -> 4, "a1" -> 9))
  }
}