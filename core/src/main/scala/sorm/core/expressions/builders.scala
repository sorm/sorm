package sorm.core.expressions.builders

import language.implicitConversions
import sorm._, core._, util._
import typeLevel.Bool, Bool._
import expressions.{templates => t}

sealed trait FromBuilder {
  type SelectSupport <: Bool
  type UpdateSupport <: Bool
  type DeleteSupport <: Bool
  type WhereSupport <: Bool
  type OrderBySupport <: Bool
  type LimitSupport <: Bool
  type OffsetSupport <: Bool
  type Template
  protected val template: Template
  protected val values: Seq[Any]
}
object FromBuilder {
  implicit def selectOps
    ( b: FromBuilder{ type SelectSupport = True; type Template <: t.Select } )
    ( implicit parser: t.Action.ResultParser[t.Action.Select[b.Template]] )
    =
    new {
      def select = {
        val template = t.Action.Select(b.template)
        val values = b.values
        Action{ runner => runner.run(template, values) }
      }
    }
  implicit def limitOps( b: FromBuilder{ type LimitSupport = True; type Template <: t.Select } ) = new {
    def limit(a: Int) = new FromBuilder {
      type SelectSupport = b.SelectSupport
      type UpdateSupport = b.UpdateSupport
      type DeleteSupport = b.DeleteSupport
      type WhereSupport = b.WhereSupport
      type OrderBySupport = b.OrderBySupport
      type LimitSupport = False
      type OffsetSupport = b.OffsetSupport
      type Template = t.Select.Limit[b.Template]
      protected val template = t.Select.Limit(b.template)
      protected val values = a +: b.values
    }
  }
  implicit def offsetOps( b: FromBuilder{ type OffsetSupport = True; type Template <: t.Select } ) = new {
    def offset(a: Int) = new FromBuilder {
      type SelectSupport = b.SelectSupport
      type UpdateSupport = b.UpdateSupport
      type DeleteSupport = b.DeleteSupport
      type WhereSupport = b.WhereSupport
      type OrderBySupport = b.OrderBySupport
      type LimitSupport = b.LimitSupport
      type OffsetSupport = False
      type Template = t.Select.Offset[b.Template]
      protected val template = t.Select.Offset(b.template)
      protected val values = a +: b.values
    }
  }
}

trait API {
  def from[ a ] =
    new FromBuilder {
      type SelectSupport = True
      type UpdateSupport = True
      type DeleteSupport = True
      type WhereSupport = True
      type OrderBySupport = True
      type LimitSupport = True
      type OffsetSupport = True
      type Template = t.Select.From[a]
      protected val template = t.Select.From[a]
      protected val values = Nil
    }
  def insert[ a ](a: a) = ???

}
