package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sext._, embrace._

import sorm._
import samples._
import org.joda.time.DateTime

@RunWith(classOf[JUnitRunner])
class MysqlDateTimeSupportSuite extends FunSuite with ShouldMatchers {
  import MysqlDateTimeSupportSuite._

  val db = TestingInstance.mysql(Entity[A]())

  val date = new DateTime()

  val a1 = db.save(A(date))
  val a2 = db.save(A(date.plusHours(3)))
  val a3 = db.save(A(date.minusSeconds(5)))
  val a4 = db.save(A(date.minusSeconds(50)))


  test("Connection now()")(pending)
  test("Larger filter"){
//  todo: generation of inserted values to drivers, because of how mysql rounds the dates
//    db.access[A].whereLarger("a", date.minusSeconds(1)).fetch()
//      .should(
//        contain(a1) and contain(a2) and not contain(a3) and not contain(a4)
//      )
  }
  test("Smaller filter")(pending)
  test("Equal filter")(pending)
  test("In filter")(pending)
  test("Other filters fail")(pending)

}
object MysqlDateTimeSupportSuite {
  case class A ( a : DateTime )
}