package sorm.mysql

trait QueryJoinTemplatesCompilers {

  import sorm.core._
  import sorm.core.util._
  import sorm.core.{expressions => i}
  import i.{templates => it}
  import sorm.{relational => o}
  import o.{queryJoinTemplates => ot}
  import java.sql.{Types => jdbcTypes}

  implicit def i1
    [ leftInputTpl <: it.Where,
      rightInputTpl <: it.Where,
      inputTpl <: it.Where.Fork[leftInputTpl, rightInputTpl, or],
      inputVals <: i.values.Where.Fork[leftInputVals, rightInputVals],
      rightInputVals,
      leftInputVals,
      or <: typeLevel.Bool,
      arg <: o.Value ]
    ( implicit 
        leftCompiler : i.Compiler[leftInputTpl, leftInputVals, ot.Where, List[arg]],
        rightCompiler : i.Compiler[rightInputTpl, rightInputVals, ot.Where, List[arg]] )
    =
    new i.Compiler[ inputTpl, inputVals, ot.Where, List[arg] ] {
      def compileTemplate(tpl: inputTpl) = {
        val left = leftCompiler.compileTemplate(tpl.left)
        val right = rightCompiler.compileTemplate(tpl.right)
        ot.Where.Fork(left, right, tpl.or.toBoolean)
      }
      def processValues(vals: inputVals) = {
        val left = leftCompiler.processValues(vals.left)
        val right = rightCompiler.processValues(vals.right)
        left ++: right
      }
    }

  implicit def i2
    [ inputTpl <: it.Where.Comparison[path, operator, negative],
      path <: TypePath,
      operator <: it.Operator.Equal,
      negative <: typeLevel.Bool,
      inputVals <: i.values.Where.Comparison[ i.values.Expression.Value[ inputValue ] ],
      inputValue <: Int ]
    =
    new i.Compiler[inputTpl, inputVals, ot.Where, List[o.Value]] {
      def compileTemplate(tpl: inputTpl) = {
        val column = {
          val name = ???
          val from = ???
          ot.Column(name, from)
        }
        val operator = ot.Operator.Equal
        val value = ot.Expression.Placeholder
        val negative = tpl.negative.toBoolean
        ot.Where.Comparison(column, operator, value, negative)
      }
      def processValues(vals: inputVals) = {
        val value = o.Value(vals.value, jdbcTypes.INTEGER)
        value +: Nil
      }
    }

}
