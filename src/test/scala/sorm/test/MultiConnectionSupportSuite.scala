package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sext._, embrace._
import sorm._
import concurrent._, duration._, ExecutionContext.Implicits._
import samples.TestingInstance

object MultiConnectionSupportSuite {
  case class A ( a : Int )
}
@RunWith(classOf[JUnitRunner])
class MultiConnectionSupportSuite extends FunSuite with ShouldMatchers with MultiDbSuite {
  import MultiConnectionSupportSuite._
  withDbs1(Set() + Entity[A](), 1 :: 14 :: Nil){ (db, name) =>
    test("Entities aren't always stored sequentially - " + name){
      val fs = (1 to 140).map(n => future(db.save(A(n))))
      val rs = fs.map(Await.result(_, 10 seconds)).sortBy(_.id)
      rs should not be ('empty)
      rs.map(_.id) should not equal (rs.map(_.a.toLong))
    }
  }
  withDbs2(Set() + Entity[A](), 1 :: 14 :: Nil){ (db, test) =>
    test("Entities aren't always stored sequentially") {
      val fs = (1 to 280).map(n => future(db.save(A(n))))
      val rs = fs.map(Await.result(_, 10 seconds))
      rs.exists(a => a.id.toInt != a.a) should be (true)
      rs.exists(a => a.id.toInt == a.a) should be (true)
    }
  }

}
