package sorm.reflection

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ScalaApiSuite extends FunSuite with ShouldMatchers {
  import ScalaApiSuite._

  import reflect.runtime.universe._
  import reflect.runtime.{currentMirror => mirror}
  import sext._
  import ScalaApi._


  test("appropriate constructors") {
    typeOf[Artist].constructors should have length (3)
  }
  test("primary constructor") {
    typeOf[Artist].constructors.head.paramss.flatten should have length (4)
  }
  test("javaClass of a local class") {
    typeOf[Artist].javaClass should equal(classOf[Artist])
  }
  test("javaClass of a wrapped in a static object class") {
    typeOf[StaticWrapper.WrappedClass].javaClass should equal(classOf[StaticWrapper.WrappedClass])
  }
  test("properties of types with mixins") {
    typeOf[Artist with Mixin].properties should equal (typeOf[Artist].properties)
  }
// //  requires different ordering of constructors
// //  test("instance works") {
// //    typeOf[Artist].instance(Map("id" -> "id1", "name" -> "name1", "genres" -> Set(), "tags" -> Set("tag1"))) should
// //      equal (Artist("id1", "name1", Set(), Set("tag1")))
// //  }

//   test("instance fails on lacking map") {
//     evaluating(typeOf[Artist].instance(Map("id" -> "id1"))) should produce [Exception]
//   }
}
object ScalaApiSuite {

  case class Genre(name: String)
  case class Artist(id: String, name: String, genres: Set[Genre], tags: Set[String]) {
    def this() = this("", "", Set(), Set())
    def this(id: String) = this(id, "", Set(), Set())
  }
  object StaticWrapper {
    class WrappedClass
  }
  class Wrapper {
    class InnerClass
  }
  trait Mixin
}
