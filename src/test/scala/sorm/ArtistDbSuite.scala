package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import samples._
import sext._

@RunWith(classOf[JUnitRunner])
class ArtistDbSuite extends FunSuite with ShouldMatchers {

  import ArtistDb._

  test("path with index"){
    Db.access[Artist]
      .whereEqual("names.value(1)", "Rolling Stones")
      .fetchOne()
      .get
      .names(Db.en)(1) should be === "Rolling Stones"
  }
  test("Offset"){
    pending
  }
  test("Limit"){
    pending
  }
  test("Ordering"){
    Db.access[Artist].order("id", true).fetch().map(_.id) should equal (6::5::4::3::2::1::Nil)
  }
  test("Contains"){
    pending
  }
  test("Equality to unpersisted entity"){
    pending
  }
  test("Equality to persisted entity"){
    Db.access[Artist]
      .whereEqual("styles.item", Db.metal)
      .fetch()
      .flatMap{_.names.values.head}
      .toSet should be === Set("Metallica", "Godsmack")
  }
  test("Map, Set, Seq deep path"){
    Db.access[Artist]
      .whereEqual("styles.item.names.value.item", "Hard Rock")
      .fetch()
      .flatMap{_.names.values.head}
      .toSet should be === Set("Metallica", "Nirvana", "Godsmack")
  }
  test("Results have correct id property"){
    Db.access[Artist].fetchOne().map{_.id} should be === Some(1)
  }
  test("Query by id"){
    Db.access[Artist].whereEqual("id", 1).fetchOne()
      .map{_.names.values.head.head}.get should be === "Metallica"
    Db.access[Artist].whereEqual("id", 3).fetchOne()
      .map{_.names.values.head.head}.get should be === "Kino"
  }

}
