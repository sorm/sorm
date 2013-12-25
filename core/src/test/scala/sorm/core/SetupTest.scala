package sorm.core

import org.scalatest.FunSuite
import sorm.core.api._
import sorm.core.util.HList

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SetupTest extends FunSuite {

  case class A(a: Int, b: String)

  test("Member is reachable") {
    val member = Setup.Member(PersistedMixiner.derive[A], Set())
    val setup = new Setup(member :: HList.Nil) {}
    import setup._

    implicitly[Setup.Member[A]]
  }

}
