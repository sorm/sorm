package vorm.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vorm._
import samples._
import extensions._

import com.codahale.logula.Logging
import org.apache.log4j.Level

@RunWith(classOf[JUnitRunner])
class ApiSampleDbTest extends FunSuite with ShouldMatchers {

  import SampleDb._


  test("results have correct id property"){
    Db.query[Artist].fetchOne().map{_.id} === 1
  }
  test("query by id"){
    Db.query[Artist].filterEquals("id", 1).fetchOne
      .map{_.names.values.head.head} === "Metallica"
    Db.query[Artist].filterEquals("id", 3).fetchOne
      .map{_.names.values.head.head} === "Kino"
  }

}
