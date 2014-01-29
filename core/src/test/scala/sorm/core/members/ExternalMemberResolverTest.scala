package sorm.core.members

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import sorm._, core._, members._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ExternalMemberResolverTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, b: String)
  case class B(c: Boolean)

  abstract class API extends members.API {
    def f( implicit stc: SomeTypeClass[ this.type, A ] ) = stc.a
  }
  object API {
    implicit def someImplicit
      [ api <: members.API, a ]
      ( implicit r: members.MemberResolver[ api, a ] )
      = new SomeTypeClass[api, a]{ override val a = true }
  }

  trait SomeTypeClass[api, a]{ val a = false }

  object db extends API {
    protected val members = membersFromTuple(member[A], member[B])
  }

  test("Member is reachable") {
    db.f should be(true)
  }

}
