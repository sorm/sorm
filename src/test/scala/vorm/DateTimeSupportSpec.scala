package vorm

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import vorm._
import dropAll._
import persisted._
import query._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import create._
import drop._
import extensions._

import samples._

@RunWith(classOf[JUnitRunner])
class DateTimeSupportSpec extends FunSpec with ShouldMatchers {

  import SampleDb._

  describe("Api date"){
    it("should return a valid date") {
      pending
    }
  }
  describe("When used in filter"){
  }
  describe("A date property of a class"){

  }

}
