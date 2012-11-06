package sorm.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import concurrent._, duration._
import concurrent.ExecutionContext.Implicits._

@RunWith(classOf[JUnitRunner])
class MysqlMultiConnectionSuite extends FunSuite with ShouldMatchers {
  import MysqlMultiConnectionSuite._

  test("Parallelly created entities arent always sequential"){
    val db = new Instance( Set() + Entity[A](), "jdbc:mysql://localhost/test", poolSize = 14, initMode = InitMode.DropAllCreate )
    val fs = (1 to 140).map(n => future(db.save(A(n))))
    val rs = fs.map(Await.result(_, 1 seconds))
    rs.exists(a => a.id.toInt != a.a) should be (true)
    rs.exists(a => a.id.toInt == a.a) should be (true)
  }

}
object MysqlMultiConnectionSuite {
  case class A ( a : Int )
  case class B ( a : Some[A] )
}