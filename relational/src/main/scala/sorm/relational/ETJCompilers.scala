package sorm.relational

import sorm._
import core._
import core.static._
import core.util._
import core.{expressions => i}
import i.{templates => it}
import i.{values => iv}
import relational._
import relational.{joinExpressions => o}
import o.{templates => ot}
import java.sql.{Types => jdbcTypes}


object ETJCompilers {

  /**
   * A compiler from `core.expressions` to `relational.joinExpressions`.
   */
  private type ETJCompiler[engine, inputTemplate, inputValues, outputTemplate] = engine.Compiler[engine, inputTemplate, inputValues, outputTemplate, List[Value]]
  
  trait Fork {
    private type InputTemplate[left <: it.Condition, right <: it.Condition] = it.Condition.Fork[left, right, typeLevel.Bool]
    private type InputValues[left, right] = iv.Condition.Fork[left, right]
    implicit def forkETJCompiler
      [ engine,
        leftInputTemplate <: it.Condition,
        rightInputTemplate <: it.Condition,
        leftInputValues <: iv.Condition,
        rightInputValues <: iv.Condition ]
      ( implicit
          leftCompiler: ETJCompiler[engine, leftInputTemplate, leftInputValues, ot.Where],
          rightCompiler: ETJCompiler[engine, rightInputTemplate, rightInputValues, ot.Where] )
      =
      new ETJCompiler[engine, InputTemplate[leftInputTemplate, rightInputTemplate], InputValues[leftInputValues, rightInputValues], ot.Where] {
        def renderTemplate( tpl: InputTemplate[leftInputTemplate, rightInputTemplate] ) = {
          val left = leftCompiler.renderTemplate(tpl.left)
          val right = rightCompiler.renderTemplate(tpl.right)
          ot.Where.Fork(left, right, tpl.or.toBoolean)
        }
        def arrangeValues(vals: InputValues[leftInputValues, rightInputValues]) = {
          val left = leftCompiler.arrangeValues(vals.left)
          val right = rightCompiler.arrangeValues(vals.right)
          left ++: right
        }
      }
  }

  trait PrimitiveEqual {
    private type InputTemplate[root, path <: TypePath[root]] = it.Condition.Comparison[root, path, it.Operator.Equal, typeLevel.Bool]
    private type InputValues[value] = iv.Condition.Comparison[ value ]
    implicit def primitiveEqualETJCompiler
      [ engine, root, path <: TypePath[root], value ]
      ( implicit
          mappingResolver: rules.MappingResolver[path],
          support: Support[PrimitiveEqual, value] )
      = {
        val mapping = mappingResolver.mapping
        new ETJCompiler[ engine, InputTemplate[root, path], InputValues[value], ot.Where ]{
          override def renderTemplate(tpl: InputTemplate[root, path]) = {
            val column = o.helpers.column(mapping).getOrElse(bug("Mapping produces no column"))
            val operator = ot.Operator.Equal
            val value = ot.Expression.Placeholder
            val negative = tpl.negative.toBoolean
            ot.Where.Comparison(column, value, operator, negative)
          }
          override def arrangeValues(vals: InputValues[value]) = {
            val value = {
              val value = vals.value
              val jdbcType = mapping.jdbcType.getOrElse(bug("Mapping produces no jdbcType"))
              Value(value, jdbcType)
            }
            value +: Nil
          }
        }
      }
  }

  /**
   * An evidence of support for `spec` by `value`.
   */
  protected class Support[spec, value]
  trait IntEqual extends PrimitiveEqual {
    protected implicit val intPrimitiveEqualSupport = new Support[PrimitiveEqual, Int]
  }
  trait BooleanEqual extends PrimitiveEqual {
    protected implicit val booleanPrimitiveEqualSupport = new Support[PrimitiveEqual, Boolean]
  }

  trait All
    extends Fork
    with IntEqual
    with BooleanEqual

}