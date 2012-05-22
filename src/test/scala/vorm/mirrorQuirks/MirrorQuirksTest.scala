package vorm.mirrorQuirks

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import vorm.mirrorQuirks.MirrorQuirksTest._
import vorm.reflection._

class MirrorQuirksTest extends FunSuite with ShouldMatchers {

  test("isInner") {
    isInner(tag[NestedClasses#NestedClass].sym) should be(true)
    isInner(tag[NestedClasses#NestedClass#DeeplyNestedClass].sym) should be(true)
    isInner(tag[Genre].sym) should be(false)
    isInner(tag[MirrorQuirksTest].sym) should be(false)
  }

  test("fullname of nested class") {
    fullName(tag[NestedClasses#NestedClass].sym) should
    be("vorm.mirrorQuirks.MirrorQuirksTest.NestedClasses#NestedClass")
  }
  test("fullname of deeply nested class") {
    fullName(tag[NestedClasses#NestedClass#DeeplyNestedClass].sym) should
    be("vorm.mirrorQuirks.MirrorQuirksTest.NestedClasses#NestedClass#DeeplyNestedClass")
  }

  test("name of type with mixin") {
    tpe[Artist with Persisted].name should equal(tpe[Artist].name)
  }
}
object MirrorQuirksTest {

  trait Persisted
  case class Artist(name: String, genres: Set[Genre])

  case class Genre(name: String)

  class NestedClasses {
    class NestedClass {
      class DeeplyNestedClass
    }
  }

}