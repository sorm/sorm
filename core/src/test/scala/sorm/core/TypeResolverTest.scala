package sorm.core

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import reflect.runtime.{universe => ru}
import static._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class TypeResolverTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, b: String)
  case class B(a: (Int, Option[A]))

  type PathToB = TypePath.Root[B]
  type PathToPropertyA = TypePath.Property[B, PathToB, shapeless._0]
  type PathToOption = TypePath.Property[B, PathToPropertyA, shapeless.nat._1]
  type PathToA = TypePath.Generic[B, PathToOption, shapeless._0]
  type PathToPropertyB = TypePath.Property[B, PathToA, shapeless.nat._1]


  test("Root") {
    implicitly[TypeResolver[PathToB]].head.shouldBe(ru.typeOf[B])
  }

  test("Case class property") {
    implicitly[TypeResolver[PathToPropertyA]].head.shouldBe(ru.typeOf[(Int, Option[A])])
  }

  test("Tuple member") {
    implicitly[TypeResolver[PathToOption]].head.shouldBe(ru.typeOf[Option[A]])
  }

  test("Option item") {
    implicitly[TypeResolver[PathToA]].head.shouldBe(ru.typeOf[A])
  }

  test("Non first case class property") {
    implicitly[TypeResolver[PathToPropertyB]].head.shouldBe(ru.typeOf[String])
  }

}
