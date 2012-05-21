package vorm.extensions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OrderedMapTest extends FunSuite with ShouldMatchers {
  test("preserves order of creation") {
    val elems = (1 to 100) zip (1 to 100)
    OrderedMap(elems: _*).toSeq should equal(elems)
  }
  test("preserves order of addition") {
    val elems1 = (1 to 100) zip (1 to 100)
    val elems2 = (101 to 200) zip (101 to 200)
    (OrderedMap(elems1: _*) ++ OrderedMap(elems2: _*) toSeq) should equal(elems1 ++ elems2)
  }
}
