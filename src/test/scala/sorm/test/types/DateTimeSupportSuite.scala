package sorm.test.types

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sext._, embrace._

import sorm._
import samples._
import org.joda.time.DateTime
import sorm.test.MultiInstanceSuite

@RunWith(classOf[JUnitRunner])
class DateTimeSupportSuite extends FunSuite with ShouldMatchers with MultiInstanceSuite {
  import DateTimeSupportSuite._

  def entities = Set() + Entity[A]()
  instancesAndIds foreach { case (db, dbId) =>

    test(dbId + " - now()")(pending)
    test(dbId + " - Larger filter and multiple attempts"){
      200 times {
        //  time rounded to seconds (for mysql compatibility)
        val date = new DateTime((db.nowMillis() / 1000d).round * 1000)
        val a1 = db.save(A(date))
        val a2 = db.save(A(date.plusHours(3)))
        val a3 = db.save(A(date.minusSeconds(5)))
        val a4 = db.save(A(date.minusSeconds(50)))

        db.query[A].whereLarger("a", date.minusSeconds(2)).fetch()
          .should(
            contain(a1) and contain(a2) and not contain(a3) and not contain(a4)
          )

        db.delete(a1)
        db.delete(a2)
        db.delete(a3)
        db.delete(a4)
      }
    }
    test(dbId + " - Smaller filter")(pending)
    test(dbId + " - Equal filter")(pending)
    test(dbId + " - In filter")(pending)
    test(dbId + " - Other filters fail")(pending)
  }
}
object DateTimeSupportSuite {
  case class A ( a : DateTime )

}
