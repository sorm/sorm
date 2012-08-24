package sorm.extensions

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import collection.immutable.Queue

@RunWith(classOf[JUnitRunner])
class OrderedMapTest extends FunSuite with ShouldMatchers {
  test("preserves order of creation") {
    val elems = (1 to 100) zip (1 to 100)
    OrderedMap(elems: _*).toSeq should equal (elems)
  }
  test("preserves order of addition") {
    val elems1 = (1 to 100) zip (1 to 100)
    val elems2 = (101 to 200) zip (101 to 200)
    (OrderedMap(elems1: _*) ++ OrderedMap(elems2: _*) toSeq) should equal(elems1 ++ elems2)
  }
  test("equals equal and have same hashCode") {
    val elems = (1 to 100) zip (1 to 100)

    OrderedMap(elems: _*) should equal (OrderedMap(elems: _*))
    OrderedMap(elems: _*).hashCode should equal (OrderedMap(elems: _*).hashCode)
  }
  test("differently ordered don't equal and have different hashcodes") {
    val elems1 = (1 to 100) zip (1 to 100)
    val elems2 = elems1.reverse

    OrderedMap(elems1: _*) should not equal (OrderedMap(elems2: _*))
    OrderedMap(elems1: _*).hashCode should not equal (OrderedMap(elems2: _*).hashCode)
  }
}
