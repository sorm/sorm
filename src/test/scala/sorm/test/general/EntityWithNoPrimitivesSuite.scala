package sorm.test.general

import sorm._
import org.scalatest._

class EntityWithNoPrimitivesSuite extends FunSuite with ShouldMatchers {
  import EntityWithNoPrimitivesSuite._

  test("All is fine"){
    val db = new Instance(
      entities = Set(
        Entity[Artist]()
      ),
      url = "jdbc:h2:mem:test",
      user = "",
      password = "",
      initMode = InitMode.DropAllCreate
    )
  
    db.save(Artist(Set("metal"),Set("foo"),Set("bar")))
  
    db.query[Artist]
      .whereEqual("id", 1)
      .fetchOne()
      .map(a => a.copy(genres = Set("rock","rnb"), a = Set("myfoo"), b = Set("mybar")))
      .map(db.save)

    db.query[Artist].whereEqual("id", 1).fetchOne().map(_.b).shouldBe(Some(Set("mybar")))
  }
}
object EntityWithNoPrimitivesSuite {
  case class Artist( genres: Set[String] , a: Set[String], b: Set[String] )

}