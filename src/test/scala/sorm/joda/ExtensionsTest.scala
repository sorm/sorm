package sorm.joda

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtensionsTest extends FunSuite with ShouldMatchers {
  import Extensions._
  import org.joda.time._

  test("Middle ages LocalDate back and forth"){
    new LocalDate(1200, 1, 1).toJava.toJoda shouldBe new LocalDate(1200, 1, 1)
  }
  test("Middle ages LocalDate toJava"){
    new LocalDate(1200, 1, 1).toJava.toString shouldBe "1200-01-01"
  }

}
