package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import sext._, embrace._

@RunWith(classOf[JUnitRunner])
class CurrentDateTimeTest extends FunSuite with ShouldMatchers {
  
  TestingInstances.instances(Set()) foreach { case (db, dbId) =>
    test(dbId + " - should be changing"){
      val a = db.now()
      Thread.sleep(1500)
      val b = db.now()
      (b.getMillis - a.getMillis) should be >= 1500l
    }
  }
}
