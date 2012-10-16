package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sext._

import samples._
import org.joda.time.DateTime

@RunWith(classOf[JUnitRunner])
class DateTimeSupportSuite extends FunSuite with ShouldMatchers {
  import sorm.DateTimeSupportSuite._

  val db = TestingInstance.h2(Sorm.Entity[A]())

  val date = new DateTime()

  val a1 = db.save(A(date))
  val a2 = db.save(A(date.plusHours(3)))
  val a3 = db.save(A(date.minusSeconds(5)))
  val a4 = db.save(A(date.minusSeconds(50)))


  test("Api dateTime")(pending)
  test("Larger filter"){
    db.access[A].whereLarger("a", date.minusSeconds(1)).fetch()
      .should(
        contain(a1) and contain(a2) and not contain(a3) and not contain(a4)
      )
  }
  test("Smaller filter")(pending)
  test("Equal filter")(pending)
  test("In filter")(pending)
  test("Other filters fail")(pending)

}
object DateTimeSupportSuite {
  case class A ( a : DateTime )
}