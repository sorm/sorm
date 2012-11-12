package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import sext._, embrace._

@RunWith(classOf[JUnitRunner])
class CurrentDateTimeTest extends FunSuite with ShouldMatchers with MultiInstanceSuite {
  
  def entities = Set()
  instancesAndIds foreach { case (db, dbId) =>
    test(dbId + " - should be changing"){
      val a = db.now()
      Thread.sleep(1500)
      val b = db.now()
      (b.getMillis - a.getMillis) should be >= 1500l
    }
  }
}
