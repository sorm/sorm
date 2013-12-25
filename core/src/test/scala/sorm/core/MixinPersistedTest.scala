package sorm.core

import org.scalatest.FunSuite
import sorm.core.api._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MixinPersistedTest extends FunSuite {

  // Having the following class:
  case class A(a: Int, b: String)

  // the following value:
  val value = A(2, "ABC")

  // the following `PersistedMixiner` instance:
  val persistedMixiner = PersistedMixiner.derive[A]

  //  should satisfy the following tests:
  test("Has the id property and it's set appropriately") {
    assert(persistedMixiner.mixinPersisted(value, 1).id == 1)
  }
  test("A copy retains the id property") {
    assert(persistedMixiner.mixinPersisted(value, 2).copy(a = 50).id == 2)
  }
  test("A copy correctly updates properties") {
    assert(persistedMixiner.mixinPersisted(value, 2).copy(a = 50).a == 50)
  }

}
