package sorm.relational

import sorm.core._
import sorm.relational.joinExpressions._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class JoinExpressionsTest extends FunSuite with ShouldMatchers {

  object instances extends joinExpressions.compilers.All
  import instances._

  case class A(a: Int)

  test("Int Equal Compiler") {

    type Path = static.TypePath.Property[A, static.TypePath.Root[A], shapeless._0]
    type InputTemplate = expressions.templates.Where.Comparison[A, Path, expressions.templates.Operator.Equal, util.typeLevel.Bool.True]
    type InputValues = expressions.values.Where.Comparison[expressions.values.Expression.Value[Int]]
    type OutputTemplate = joinExpressions.templates.Where
    type OutputValues = List[Value]
    type Compiler = expressions.Compiler[InputTemplate, InputValues, OutputTemplate, OutputValues]

    val inputTemplate : InputTemplate = {
      val path = null : Path
      val operator = expressions.templates.Operator.Equal()
      val negative = util.typeLevel.Bool.True()
      expressions.templates.Where.Comparison(path, operator, negative)
    }
    val outputTemplate = {
      import joinExpressions.templates._
      val column = {
        val from = From.Root("a")
        Column("a", from)
      }
      val value = Expression.Placeholder
      val operator = Operator.Equal
      val negative = true
      Where.Comparison(column, value, operator, negative)
    }


    implicitly[Compiler].compileTemplate(inputTemplate).shouldBe(outputTemplate)

  }

}
