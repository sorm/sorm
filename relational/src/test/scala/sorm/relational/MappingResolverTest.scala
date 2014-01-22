package sorm.relational

import sorm.core._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import rules._
import static.TypePath

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MappingResolverTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, b: String)
  case class B(a: (Int, Option[A]))

  type Root = TypePath.Root[B]
  type PathToPropertyAOfTypeB = TypePath.Property[B, Root, shapeless._0]
  type PathToSecondTupleItemOfPropertyAOfTypeB = TypePath.Property[B, PathToPropertyAOfTypeB, shapeless.nat._1]
  type PathToTypeA = TypePath.Generic[B, PathToSecondTupleItemOfPropertyAOfTypeB, shapeless._0]
  type PathToPropertyBOfTypeA = TypePath.Property[B, PathToTypeA, shapeless.nat._1]

  test("Root") {
    implicitly[MappingResolver[Root]].mapping.t.typeSymbol.name.toString.shouldBe("B")
  }

  test("Generic") {
    implicitly[MappingResolver[PathToTypeA]].mapping.t.typeSymbol.name.toString.shouldBe("A")
  }

  test("Property") {
    implicitly[MappingResolver[PathToPropertyBOfTypeA]].mapping.t.typeSymbol.name.toString.shouldBe("String")
  }


}
