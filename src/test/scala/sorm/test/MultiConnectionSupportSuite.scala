package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sext._, embrace._
import sorm._
import concurrent._, duration._, ExecutionContext.Implicits._

object MultiConnectionSupportSuite {
  case class A ( a : Int )
}
@RunWith(classOf[JUnitRunner])
class MultiConnectionSupportSuite extends FunSuite with ShouldMatchers {
  import MultiConnectionSupportSuite._

  TestingInstances.instances(Set() + Entity[A](), 1 :: 14 :: Nil) foreach { case (db, dbId) =>
    test(dbId + " - Entities aren't always stored sequentially"){
      val fs = (1 to 140).map(n => future(db.save(A(n))))
      val rs = fs.map(Await.result(_, 10 seconds)).sortBy(_.id)
      rs should not be ('empty)
      rs.map(_.id) should not equal (rs.map(_.a.toLong))
    }
  }

}
