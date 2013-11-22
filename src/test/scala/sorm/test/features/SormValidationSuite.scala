package sorm.test.features

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import sorm._
import samples._
import sorm.{Entity, Instance}

@RunWith(classOf[JUnitRunner])
class SormValidationSuite extends FunSuite with ShouldMatchers {
  import SormValidationSuite._

  test("Mutually recursive types are not supported"){
    evaluating {
      new Instance(
        Entity[F]() :: Entity[G]() :: Nil,
        "jdbc:h2:mem:test",
        initMode = InitMode.DropAllCreate
      ).close()
    } should produce [Instance.ValidationException]
  }
  test("Mutually recursive types are not supported - deep"){
    evaluating {
      new Instance(
        Entity[F]() :: Entity[H]() :: Nil,
        "jdbc:h2:mem:test",
        initMode = InitMode.DropAllCreate
      ).close()
    } should produce [Instance.ValidationException]
  }
  test("Recursive types are not supported"){
    evaluating {
      new Instance(
        Entity[E]() :: Nil,
        "jdbc:h2:mem:test",
        initMode = InitMode.DropAllCreate
      ).close()
    } should produce [Instance.ValidationException]
  }
  test("`Any` type is not supported"){
    evaluating {
      new Instance(
        Entity[D]() :: Nil,
        "jdbc:h2:mem:test",
        initMode = InitMode.DropAllCreate
      ).close()
    } should produce [Instance.ValidationException]
  }
  test("referred entities validation"){
    evaluating {
      new Instance(
        Entity[A]() :: Nil,
        "jdbc:h2:mem:test",
      initMode = InitMode.DropAllCreate
      ).close()
    } should produce [Instance.ValidationException]
  }
  test("Correct instantiation doesn't throw exceptions"){
    new Instance(
      Entity[A]() :: Entity[B]() :: Entity[C]() :: Nil,
      "jdbc:h2:mem:test",
      initMode = InitMode.DropAllCreate
    ).close()
  }
}
object SormValidationSuite {
  case class A
    ( a : Seq[Option[(B, Int)]], b : B, c : Seq[C] )
  case class B
    ( a : Int, b : C )
  case class C
    ( a : Int )
  case class D
    ( a : Seq[Any] )
  case class E
    ( a : Seq[E] )
  case class F(a: Seq[G])
  case class G(a: F)
  case class H(a: Seq[G])
}