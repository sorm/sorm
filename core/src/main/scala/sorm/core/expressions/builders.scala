package sorm.core.expressions.builders

import sorm._, core._, util._, expressions._
import expressions.{templates => t}

trait API {

  protected def selectParser[ a ]: ResultParser[ Iterable[ a with api.Persisted ] ]

  protected val expressionsRunner: Runner

  def from[a] = {
    val template = t.Select.From[a]()
    type select = t.Select.From[a]
    new Builder(template, Nil) with Select[select] with Limit[select] with Offset[select]
  }

  sealed abstract class Builder
    [ template ]
    ( protected val template: template,
      protected val values: Seq[Any] )

  sealed trait Select[ template <: t.Select ] extends Builder[template] {
    def select
      ( implicit rootResolver: t.Select.RootResolver[template] )
      = {
        val template = t.Action.Select(this.template)
        val values = this.values
        expressionsRunner.run(template, values)(selectParser[rootResolver.Root])
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
