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
    val a = db.now()
    Thread.sleep(1500)
    val b = db.now()
    (b.getMillis - a.getMillis) should be >= 1500l
  }
}
