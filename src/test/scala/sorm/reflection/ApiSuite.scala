package sorm.reflection

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ApiSuite extends FunSuite with ShouldMatchers {
  import ApiSuite._

  test("String signature"){
    Reflection[String].signature should be === "java.lang.String"
  }
  test("Int inheritance"){
    (Reflection[Int] inheritsFrom Reflection[AnyVal]) should equal (true)
    (Reflection[Int] inheritsFrom Reflection[Any]) should equal (true)
    (Reflection[Int] inheritsFrom Reflection[Int]) should equal (true)
    (Reflection[Int] inheritsFrom Reflection[AnyRef]) should equal (false)
  }
  test("String inheritance"){
    assert( Reflection[String] inheritsFrom Reflection[Any] )
    assert( Reflection[String] inheritsFrom Reflection[String] )
  }
  test("Generic type does not inherit from same basis with other generics"){
    assert( !Reflection[Seq[Int]].inheritsFrom(Reflection[Seq[String]]) )
  }
  test("Generic type inherits higher basis type with same generics"){
    assert( Reflection[Seq[Int]].inheritsFrom(Reflection[Traversable[Int]]) )
  }
  test("Generic type inherits higher generics"){
    assert( Reflection[Seq[Seq[Int]]].inheritsFrom(Reflection[Seq[Traversable[Int]]]) )
    assert( Reflection[Seq[Seq[Int]]].inheritsFrom(Reflection[Seq[Traversable[Any]]]) )
  }
  test("Generics must be proper"){
    assert( Reflection[Seq[Int]].generics(0) =:= Reflection[Int] )
  }
  test("Instantiation by map"){
    Reflection[Artist]
      .instantiate(
        Map("genres" -> Set(), "tags" -> Set(), "name" -> "X", "id" -> 99)
      )
      .should(
        equal( Artist(99, "X", Set(), Set()) )
      )
  }
  test("Instantiation fails on improper arguments"){
    evaluating {
      Reflection[Artist]
        .instantiate(
          Map("tags" -> Set(), "name" -> "X", "id" -> 99)
        )
    } should produce [Throwable]
  }

  test("property value") {
    val artist = Artist(234, "Nirvana", Set(Genre("Grunge"), Genre("Rock")), Set("kurt-cobain", "grunge", "nirvana"))

    Reflection[Artist].propertyValue("name", artist) should equal("Nirvana")
  }
  test("signature of class with deep generics") {
    Reflection[Map[Set[String], Int]].signature should
      equal("scala.collection.immutable.Map[scala.collection.immutable.Set[java.lang.String], scala.Int]")
  }
//  test("signature of class with empty generics") {
//    Reflection[Seq[_]].signature should
//      equal("scala.collection.Seq[scala.Any]")
//  }
  test("signature of class with no generics") {
    Reflection[Int].signature should
      equal("scala.Int")
  }
  test("inner class fullName") {
    Reflection[Wrapper#InnerClass].fullName should equal("sorm.reflection.ApiSuite.Wrapper#InnerClass")
  }

}
object ApiSuite {

  case class Genre(name: String)
  case class Artist(id: Int, name: String, genres: Set[Genre], tags: Set[String]) {
    def this() = this(0, "", Set(), Set())
    def this(id: Int) = this(id, "", Set(), Set())
  }
  object StaticWrapper {
    class WrappedClass
  }
  class Wrapper {
    class InnerClass
  }
  trait Mixin
}
