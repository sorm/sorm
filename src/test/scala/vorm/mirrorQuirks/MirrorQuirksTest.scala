package vorm.mirrorQuirks

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import vorm.mirrorQuirks.MirrorQuirksSuite._

class MirrorQuirksSuite extends FunSuite with ShouldMatchers {

  test("isInner") {
    isInner(tag[NestedClasses#NestedClass].sym) should be(true)
    isInner(tag[NestedClasses#NestedClass#DeeplyNestedClass].sym) should be(true)
    isInner(tag[Genre].sym) should be(false)
    isInner(tag[MirrorQuirksSuite].sym) should be(false)
  }

  test("fullname of nested class") {
    fullName(tag[NestedClasses#NestedClass].sym) should
      be("vorm.mirrorQuirks.MirrorQuirksSuite.NestedClasses#NestedClass")
  }
  test("fullname of deeply nested class") {
    fullName(tag[NestedClasses#NestedClass#DeeplyNestedClass].sym) should
      be("vorm.mirrorQuirks.MirrorQuirksSuite.NestedClasses#NestedClass#DeeplyNestedClass")
  }

}
object MirrorQuirksSuite {

  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)

  class NestedClasses {
    class NestedClass {
      class DeeplyNestedClass
    }
  }

}