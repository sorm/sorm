package sorm.test.issues

import org.scalatest._

@org.junit.runner.RunWith(classOf[junit.JUnitRunner])
class Issue40Test extends FunSuite with ShouldMatchers {
  import sorm._
  import Issue40Test._

  test("All is fine"){
    new Instance (
      entities = Set(Entity[Users]()),
      url = "jdbc:h2:file:target/test/db/users",
      initMode = InitMode.Create,
      poolSize = 20
    )
    new Instance (
      entities = Set(Entity[Users]()),
      url = "jdbc:h2:file:target/test/db/users",
      initMode = InitMode.Create,
      poolSize = 20
    )
  }

}
object Issue40Test {
  case class Users()
}