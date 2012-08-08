package vorm.reflection

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TypeTest extends FunSuite with ShouldMatchers {
  import TypeTest._

  class InnerClass

  test("appropriate constructors") {
    tpe[Artist].constructors should have length (3)
  }
  test("primary constructor") {
    tpe[Artist].constructors.head.arguments should have length (4)
  }
  test("javaClass of a local class") {
    tpe[Artist].javaClass should equal(classOf[Artist])
  }
  test("property value") {
    val artist = Artist("234", "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))

    tpe[Artist].propertyValue("name", artist) should equal("Nirvana")
  }
  test("javaClass of a wrapped in a static object class") {
    tpe[StaticWrapper.WrappedClass].javaClass should equal(classOf[StaticWrapper.WrappedClass])
  }
  test("signature of class with deep generics") {
    tpe[Map[Set[String], Int]].signature should
      equal("scala.collection.immutable.Map[scala.collection.immutable.Set[java.lang.String], scala.Int]")
  }
  test("signature of class with empty generics") {
    tpe[Seq[_]].signature should
      equal("scala.collection.Seq[scala.Any]")
  }
  test("signature of class with no generics") {
    tpe[Int].signature should
      equal("scala.Int")
  }
  test("inner class fullName") {
    tpe[InnerClass].fullName should equal("vorm.reflection.TypeTest#InnerClass")
  }
  test("properties of types with mixins") {
    tpe[Artist with Mixin].properties should equal (tpe[Artist].properties)
  }
//  requires different ordering of constructors
//  test("instance works") {
//    tpe[Artist].instance(Map("id" -> "id1", "name" -> "name1", "genres" -> Set(), "tags" -> Set("tag1"))) should
//      equal (Artist("id1", "name1", Set(), Set("tag1")))
//  }

  test("instance fails on lacking map") {
    evaluating(tpe[Artist].instance(Map("id" -> "id1"))) should produce [Exception]
  }
}
object TypeTest {

  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def this() = this("", "", Set(), Set())
    def this(id: String) = this(id, "", Set(), Set())
  }
  object StaticWrapper {
    class WrappedClass
  }
  trait Mixin
}
