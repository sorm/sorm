package vorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vorm._
import api._
import persisted._
import query._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import create._
import extensions._

import samples._

@RunWith(classOf[JUnitRunner])
class SeqSupportSuite extends FunSuite with ShouldMatchers {

  import ArtistDb._

  test("equals filter"){
    // Db.query[Artist].filterEquals("names.value")
  }
}
