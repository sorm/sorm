package sorm.core

import org.scalatest.FunSuite
import sorm.core.api._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SetupTest extends FunSuite {

  case class A(a: Int, b: String)

  test("Member is reachable") {

    val members = new Setup.Members(Tuple1(new Setup.Member(PersistedMixiner.derive[A], Set(), Set())))

    import members._
    implicitly[Setup.Member[A]]
  }

}
