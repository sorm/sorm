package sorm.mirrorQuirks

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import sorm.mirrorQuirks.MirrorQuirksTest._
import MirrorQuirks._

class MirrorQuirksTest extends FunSuite with ShouldMatchers {

  test("isInner") {
    isInner(tag[NestedClasses#NestedClass].sym) should be(true)
    isInner(tag[NestedClasses#NestedClass#DeeplyNestedClass].sym) should be(true)
    isInner(tag[Genre].sym) should be(false)
    isInner(tag[MirrorQuirksTest].sym) should be(false)
  }
  test("fullname of nested class") {
    fullName(tag[NestedClasses#NestedClass].sym) should
    be("sorm.mirrorQuirks.MirrorQuirksTest.NestedClasses#NestedClass")
  }
  test("fullname of deeply nested class") {
    fullName(tag[NestedClasses#NestedClass#DeeplyNestedClass].sym) should
    be("sorm.mirrorQuirks.MirrorQuirksTest.NestedClasses#NestedClass#DeeplyNestedClass")
  }
  test("name of type with mixin") {
    name(tag[Artist with Mixin].sym) should equal(name(tag[Artist].sym))
  }
  test("properties of types with mixins") {
    println(tag[Artist with Mixin].tpe.kind)
    println(tag[Artist].tpe.kind)
    println(isMixedIn(tag[Artist with Mixin].tpe))
    properties(tag[Artist with Mixin].tpe) should equal (properties(tag[Artist].tpe))
  }

}
object MirrorQuirksTest {

  trait Mixin
  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)

  class NestedClasses {
    class NestedClass {
      class DeeplyNestedClass
    }
  }

}