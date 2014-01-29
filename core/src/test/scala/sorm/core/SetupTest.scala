package sorm.core

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import shapeless._
import members._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SetupTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, b: String)

  val aMember = new Member[A](api.PersistedMixiner.derive[A], Set(), Set())

  abstract class API[ hlist <: shapeless.HList ]( members: hlist ) extends Container {
    type MembersHList = hlist
    protected val membersHList = members
    def testMember[ a ]( implicit resolver: MemberResolver[ this.type, a ] ) = true
  }

  object db extends API( aMember :: HNil )

  test("Member is reachable") {
    db.testMember[A].should(be(true))
  }

}
