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
    Db.one[Artist]
      .filterEqual("names.value(1)", "Rolling Stones")
      .fetch()
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
    Db.all[Artist].orderDesc("id").fetch().map(_.id) should equal (6::5::4::3::2::1::Nil)
  }
  test("Contains"){
    pending
  }
  test("Equality to unpersisted entity"){
    pending
  }
  test("Equality to persisted entity"){
    Db.all[Artist]
      .filterEqual("styles.item", Db.metal)
      .fetch()
      .flatMap{_.names.values.head}
      .toSet should be === Set("Metallica", "Godsmack")
  }
  test("Map, Set, Seq deep path"){
    Db.all[Artist]
      .filterEqual("styles.item.names.value.item", "Hard Rock")
      .fetch()
      .flatMap{_.names.values.head}
      .toSet should be === Set("Metallica", "Nirvana", "Godsmack")
  }
  test("Results have correct id property"){
    Db.one[Artist].fetch().map{_.id} should be === Some(1)
  }
  test("Query by id"){
    Db.one[Artist].filterEqual("id", 1).fetch()
      .map{_.names.values.head.head}.get should be === "Metallica"
    Db.one[Artist].filterEqual("id", 3).fetch()
      .map{_.names.values.head.head}.get should be === "Kino"
  }

}
