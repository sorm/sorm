package sorm

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
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
class DateTimeSupportSuite extends FunSuite with ShouldMatchers {


  test("Api fetchDate")(pending)
  test("Bigger filter")(pending)
  test("Smaller filter")(pending)
  test("Equals filter")(pending)
  test("In filter")(pending)
  test("Other filters fail")(pending)

}
