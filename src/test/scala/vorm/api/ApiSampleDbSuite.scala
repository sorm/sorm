package vorm.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vorm._
import extensions._

import samples.SampleDb._

@RunWith(classOf[JUnitRunner])
class ApiSampleDbSuite extends FunSuite with ShouldMatchers {

  test("Ordering"){
    pending
  }
  test("Contains"){
    pending
  }
  test("Equality to unpersisted entity"){
    pending
  }
  test("Equality to persisted entity"){
    Db.query[Artist]
      .filterEquals("styles.item", Db.metal)
      .fetchAll
      .flatMap{_.names.values.head.head}
      .toSet === Set("Metallica", "Godsmack")
  }
  test("Map, Set, Seq deep path"){
    Db.query[Artist]
      .filterEquals("styles.item.names.value.item", "Hard Rock")
      .fetchAll
      .flatMap{_.names.values.head.head}
      .toSet === Set("Metallica", "Nirvana", "Godsmack")
  }
  test("Results have correct id property"){
    Db.query[Artist].fetchOne.map{_.id} === 1
  }
  test("Query by id"){
    Db.query[Artist].filterEquals("id", 1).fetchOne
      .map{_.names.values.head.head} === "Metallica"
    Db.query[Artist].filterEquals("id", 3).fetchOne
      .map{_.names.values.head.head} === "Kino"
  }

}
