package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sext._, embrace._
import sorm._
import concurrent._, duration._, ExecutionContext.Implicits._

@RunWith(classOf[JUnitRunner])
class MysqlMultiConnectionSuite extends FunSuite with ShouldMatchers {
  import MysqlMultiConnectionSuite._

  test("Entities created on multiple connections arent always sequential"){
    val db = new Instance( Set() + Entity[A](), "jdbc:mysql://localhost/test", poolSize = 14, initMode = InitMode.DropAllCreate )
    val fs = (1 to 2800).map(n => future(db.save(A(n))))
    val rs = fs.map(Await.result(_, 10 seconds))
    rs.exists(a => a.id.toInt != a.a) should be (true)
    rs.exists(a => a.id.toInt == a.a) should be (true)
  }

//  test("Entities created on a single connection are sequential"){
//    val db = new Instance( Set() + Entity[A](), "jdbc:mysql://localhost/test", poolSize = 1, initMode = InitMode.DropAllCreate )
//    val fs = (1 to 2800).map(n => future(db.save(A(n))))
//    Future.sequence(fs) $ (Await.result(_, 10 seconds))
//    db.access[A].order("id").fetch().map(_.a).toList should equal(1 to 28)
//  }

}
object MysqlMultiConnectionSuite {
  case class A ( a : Int )
}