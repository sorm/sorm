package sorm.core

import org.scalatest.FunSuite
import sorm.core.api._
import sorm.core.util.HList

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SetupTest extends FunSuite {

  case class A(a: Int, b: String)

// Running the following fails with
// java.lang.VerifyError: (class: sorm/core/SetupTest$$anonfun$1$$anon$1, method: <init> signature: (Lsorm/core/SetupTest$$anonfun$1;)V) Expecting to find object/array on stack
  test("Member is reachable") {
    val setup = new Setup(Setup.Member(PersistedMixiner.derive[A], Set()) :: HList.Nil) {}
    import setup._

    implicitly[Setup.Member[A]]
  }
// However, if we simply declare `member` as a new variable, as in the following, it will run fine.
//  test("Member is reachable") {
//    val member = Setup.Member(PersistedMixiner.derive[A], Set())
//    val setup = new Setup(member :: HList.Nil) {}
//    import setup._
//
//    implicitly[Setup.Member[A]]
//  }

}
