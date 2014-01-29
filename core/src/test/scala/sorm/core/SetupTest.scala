package sorm.core

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import shapeless._
import members._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SetupTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, b: String)
  case class B(c: Boolean)

  abstract class API extends members.API {
    def testMember[ a ]( implicit resolver: MemberResolver[ a ] ) = true
  }

  object db extends API {
    protected val members = membersFromTuple(member[A], member[B])
  }

  test("Member is reachable") {
    db.testMember[A].should(be(true))
  }

}
