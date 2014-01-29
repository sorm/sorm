package sorm.core

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import shapeless._
import members._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SetupTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, b: String)

  val aMember = new Member[A](api.PersistedMixiner.derive[A], Set(), Set())

  abstract class API extends members.API {
    def testMember[ a ]( implicit resolver: MemberResolver[ a ] ) = true
  }

  object db extends API {
    protected val members = Members.fromTuple(Tuple1(aMember))
  }

  test("Member is reachable") {
    db.testMember[A].should(be(true))
  }

}
