package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import sorm.samples.TestingInstance

@RunWith(classOf[JUnitRunner])
class CurrentDateTimeTest extends FunSuite with ShouldMatchers {
  val db = TestingInstance.h2()

  test("should be changing"){
    val a = db.fetchDate()
    Thread.sleep(1500)
    val b = db.fetchDate()
    (b.getMillis - a.getMillis) should equal(1500)
  }
}
