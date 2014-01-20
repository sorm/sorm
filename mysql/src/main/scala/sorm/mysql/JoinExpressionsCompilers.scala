package sorm.mysql

trait JoinExpressionsCompilers {

  import sorm.core._
  import sorm.core.util._
  import sorm.core.{expressions => genExp}
  import sorm.{relational => rel}
  import rel.{joinExpressions => relExp}
  import java.sql.{Types => jdbcTypes}
  
  implicit def i1
    [ leftInputTpl <: genExp.templates.Where,
      rightInputTpl <: genExp.templates.Where,
      inputTpl <: genExp.templates.Where.Fork[leftInputTpl, rightInputTpl, or],
      inputVals <: genExp.values.Where.Fork[leftInputVals, rightInputVals],
      rightInputVals,
      leftInputVals,
      or <: typeLevel.Bool,
      arg <: rel.Value ]
    ( implicit 
        leftCompiler : genExp.Compiler[leftInputTpl, leftInputVals, relExp.templates.Where, List[arg]],
        rightCompiler : genExp.Compiler[rightInputTpl, rightInputVals, relExp.templates.Where, List[arg]] )
    =
    new genExp.Compiler[ inputTpl, inputVals, relExp.templates.Where, List[arg] ] {
      def compileTemplate(tpl: inputTpl) = {
        val left = leftCompiler.compileTemplate(tpl.left)
        val right = rightCompiler.compileTemplate(tpl.right)
        relExp.templates.Where.Fork(left, right, tpl.or.toBoolean)
      }
      def processValues(vals: inputVals) = {
        val left = leftCompiler.processValues(vals.left)
        val right = rightCompiler.processValues(vals.right)
        left ++: right
      }
    }

  implicit def i2
    [ inputTpl <: genExp.templates.Where.Comparison[root, path, operator, negative],
      root,
      path <: TypePath[root],
      operator <: genExp.templates.Operator.Equal,
      negative <: typeLevel.Bool,
      inputVals <: genExp.values.Where.Comparison[ genExp.values.Expression.Value[ inputValue ] ],
      inputValue <: Int ]
    // This is a frontend instance, so all we need is just resolve the column dynamically.
    // So there's no need for deeper typeclasses.
    // ( implicit
    //     pathToColumn: ToColumn[path] )
    ( implicit pathTypeResolver: TypeResolver[path] )
    =
    new genExp.Compiler[inputTpl, inputVals, relExp.templates.Where, List[rel.Value]] {
      def compileTemplate(tpl: inputTpl) = {
        val column = relExp.functions.column(???)
        val operator = relExp.templates.Operator.Equal
        val value = relExp.templates.Expression.Placeholder
        val negative = tpl.negative.toBoolean
        relExp.templates.Where.Comparison(column, operator, value, negative)
      }
      def processValues(vals: inputVals) = {
        val value = rel.Value(vals.value, jdbcTypes.INTEGER)
        value +: Nil
      }
    }

}
