package vorm.reflection

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TypeSpec extends FunSpec with ShouldMatchers {

  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def this() = this("", "", Set(), Set())
    def this(id: String) = this(id, "", Set(), Set())
  }

  describe("A primary constructor") {
    it("should be in head") {
      tpe[Artist].constructors.head.arguments should have length (4)
    }
  }
  describe("A Seq[Int]") {
    it("should inherit Seq[Int]") {
      tpe[Seq[Int]].inherits[Seq[Int]] should be(true)
    }
  }
}
