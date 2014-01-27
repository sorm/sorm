package sorm.core.expressions.builders

import sorm._, core._, util._, expressions._
import expressions.{templates => t}

trait API[ driver ] {

  protected val expressionsRunner: Runner[ driver ]

  def from[a] = {
    val template = t.Select.From[a]()
    type select = t.Select.From[a]
    new Builder(template, Nil) with Select[select] with Limit[select] with Offset[select]
  }

  sealed abstract class Builder
    [ template ]
    ( protected val template: template,
      protected val values: Seq[Any] )

  sealed trait Select[ select <: t.Select ] extends Builder[select] {
    def select
      ( implicit parser: t.Action.ResultParser[ driver, t.Action.Select[select] ] )
      = {
        val template = t.Action.Select(this.template)
        val values = this.values
        expressionsRunner.run(template, values)
      }
  }

  sealed trait Limit[ template <: t.Select ] extends Builder[template] {
    def limit = ???
  }

  sealed trait Offset[ template <: t.Select ] extends Builder[template] {
    def offset( a: Int ) = {
      val template = t.Select.Offset(this.template)
      val values = a +: this.values
      type select = t.Select.Offset[template]
      new Builder(template, values) with Select[ select ]
    }
  }

}
