package sorm.relational.joinExpressions

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


object compilers {

  private type Compiler[inputTemplate, inputValues] = core.Compiler[inputTemplate, inputValues, ot.Where, List[Value]]
  
  trait Fork {
    private type InputTemplate[left <: it.Where, right <: it.Where] = it.Where.Fork[left, right, typeLevel.Bool]
    private type InputValues[left, right] = iv.Where.Fork[left, right]
    protected implicit def forkInstance
      [ leftInputTemplate <: it.Where,
        rightInputTemplate <: it.Where,
        leftInputValues <: iv.Where,
        rightInputValues <: iv.Where ]
      ( implicit
          leftCompiler: Compiler[leftInputTemplate, leftInputValues],
          rightCompiler: Compiler[rightInputTemplate, rightInputValues] )
      =
      new Compiler[InputTemplate[leftInputTemplate, rightInputTemplate], InputValues[leftInputValues, rightInputValues]] {
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
    private type InputTemplate[root, path <: TypePath[root]] = it.Where.Comparison[root, path, it.Operator.Equal, typeLevel.Bool]
    private type InputValues[value] = iv.Where.Comparison[ iv.Expression.Value[ value ] ]
    protected implicit def primitiveEqualInstance
      [ root, path <: TypePath[root], value ]
      ( implicit
          mappingResolver: rules.MappingResolver[path],
          support: Support[PrimitiveEqual, value] )
      = {
        val mapping = mappingResolver.mapping
        new Compiler[ InputTemplate[root, path], InputValues[value] ]{
          override def renderTemplate(tpl: InputTemplate[root, path]) = {
            val column = o.functions.column(mapping).getOrElse(bug("Mapping produces no column"))
            val operator = ot.Operator.Equal
            val value = ot.Expression.Placeholder
            val negative = tpl.negative.toBoolean
            ot.Where.Comparison(column, value, operator, negative)
          }
          override def arrangeValues(vals: InputValues[value]) = {
            val value = {
              val value = vals.expression.value
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