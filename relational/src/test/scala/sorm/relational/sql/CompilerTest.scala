package sorm.relational.sql

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import sorm._, core._, relational._, sql._, templates._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class CompilerTest extends FunSuite with ShouldMatchers {

  val compiler = new Compiler {}
  import compiler._

  test("Insert"){
    val input: Statement = Statement.Insert(
      table = Identifier("table1"),
      values = Seq(
        (Identifier("column1"), Expr.Placeholder(Placeholder)),
        (Identifier("column2"), Expr.Placeholder(Placeholder))
      )
    )
    val output = compile(input)
    val template = """INSERT INTO "table1" ("column1", "column2") VALUES (?, ?)"""
    def int( a: Int ) = Value(a, JDBCType.integer)
    val inputValues = ((), Seq(((), int(1)), ((), int(2))))
    val outputValues = Seq(int(1), int(2))

    output.template.shouldBe(template)
    output.f(inputValues).shouldBe(outputValues)

  }

}
