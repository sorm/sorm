package sorm.core

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import reflect.runtime.{universe => ru}

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ToTypeTest extends FunSuite with ShouldMatchers {

  case class A(a: Int, b: String)
  case class B(a: (Int, Seq[A]))

  type PathToB = TypePath.Root[B]
  type PathToPropertyA = TypePath.Member[B, PathToB, shapeless._0]
  type PathToSeq = TypePath.Member[B, PathToPropertyA, shapeless.nat._1]
  type PathToA = TypePath.Member[B, PathToSeq, shapeless._0]
  type PathToPropertyB = TypePath.Member[B, PathToA, shapeless.nat._1]


  test("Root") {
    implicitly[ToType[PathToB]].toType.shouldBe(ru.typeOf[B])
  }

  test("First level") {
    implicitly[ToType[PathToPropertyA]].toType.shouldBe(ru.typeOf[(Int, Seq[A])])
  }

  test("Second level") {
    implicitly[ToType[PathToSeq]].toType.shouldBe(ru.typeOf[Seq[A]])
  }

  test("Deep") {
    implicitly[ToType[PathToPropertyB]].toType.shouldBe(ru.typeOf[String])
  }

}
