package sorm.test.features

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sext._, embrace._
import sorm._
import concurrent._, duration._, ExecutionContext.Implicits._
import sorm.test.MultiInstanceSuite

object MultiConnectionSupportSuite {
  case class A ( a : Int )
}
@RunWith(classOf[JUnitRunner])
class MultiConnectionSupportSuite extends FunSuite with ShouldMatchers with MultiInstanceSuite {
  import MultiConnectionSupportSuite._

  override def entities = Entity[A]() :: Nil
  override def poolSizes = 1 :: 14 :: Nil
  instancesAndIds foreach { case (db, dbId) =>
    test(dbId + " - Entities aren't always stored sequentially"){
      val fs = (1 to 200).map(n => future(db.save(A(n))))
      val rs = fs.map(Await.result(_, 10 seconds)).sortBy(_.id)
      rs should not be ('empty)
      rs.map(_.id) should not equal (rs.map(_.a.toLong))
    }
  }

}
