package sorm.relational.joinExpressions

import sorm.core._
import sorm.core.static._
import sorm.core.util._
import sorm.core.{expressions => i}
import i.{templates => it}
import i.{values => iv}
import sorm.relational._
import sorm.relational.{joinExpressions => o}
import o.{templates => ot}
import java.sql.{Types => jdbcTypes}


object compilers {

  trait All extends Fork with IntEqual

  private type OutputValues = List[Value]
  
  trait Fork {
    private type InputTemplate[left <: it.Where, right <: it.Where] = it.Where.Fork[left, right, typeLevel.Bool]
    private type InputValues[left, right] = iv.Where.Fork[left, right]
    protected implicit def forkInstance
      [ leftInputTemplate <: it.Where,
        rightInputTemplate <: it.Where,
        leftInputValues <: iv.Where,
        rightInputValues <: iv.Where ]
      ( implicit
          leftCompiler: i.Compiler[leftInputTemplate, leftInputValues, ot.Where, OutputValues],
          rightCompiler: i.Compiler[rightInputTemplate, rightInputValues, ot.Where, OutputValues] )
      =
      new i.Compiler[InputTemplate[leftInputTemplate, rightInputTemplate], InputValues[leftInputValues, rightInputValues], ot.Where, OutputValues] {
        def compileTemplate( tpl: InputTemplate[leftInputTemplate, rightInputTemplate] ) = {
          val left = leftCompiler.compileTemplate(tpl.left)
          val right = rightCompiler.compileTemplate(tpl.right)
          ot.Where.Fork(left, right, tpl.or.toBoolean)
        }
        def processValues(vals: InputValues[leftInputValues, rightInputValues]) = {
          val left = leftCompiler.processValues(vals.left)
          val right = rightCompiler.processValues(vals.right)
          left ++: right
        }
      }
  }

  trait IntEqual {
    private type InputTemplate[root, path <: TypePath[root]] = it.Where.Comparison[root, path, it.Operator.Equal, typeLevel.Bool]
    private type InputValues = iv.Where.Comparison[ iv.Expression.Value[ Int ] ]
    protected implicit def intEqualInstance
      [ root, path <: TypePath[root] ]
      ( implicit mappingResolver: rules.MappingResolver[path] )
      = {
        new i.Compiler
          [ InputTemplate[root, path],
            InputValues,
            ot.Where,
            OutputValues ]
          {
            override def compileTemplate(tpl: InputTemplate[root, path]) = {
              val column = o.functions.column(mappingResolver.mapping).getOrElse(bug("Mapping produces no column"))
              val operator = ot.Operator.Equal
              val value = ot.Expression.Placeholder
              val negative = tpl.negative.toBoolean
              ot.Where.Comparison(column, value, operator, negative)
            }
            override def processValues(vals: InputValues) = {
              import i.values._
              val value = {
                val value = vals.expression match {
                  case Expression.Value(z) => z
                }
                Value(value, jdbcTypes.INTEGER)
              }
              value +: Nil
            }
          }
      }
  }

}