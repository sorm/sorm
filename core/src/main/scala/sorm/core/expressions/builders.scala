package sorm.core.expressions.builders

import sorm._, core._, util._, expressions._
import expressions.{templates => t}

trait API[ driver ] extends members.API { cake =>

  protected val expressionsRunner: Runner[ driver ]

  def from[a]( implicit memberResolver: MemberResolver[ a ] ) = {
    val member = memberResolver.apply
    val template = t.Select.From[a]()
    type select = t.Select.From[a]
    new Builder(member, template, Nil) with Select[select] with Limit[select] with Offset[select]
  }

  sealed abstract class Builder
    [ template ]
    ( protected val member: core.members.Member,
      protected val template: template,
      protected val values: Seq[Any] )

  sealed trait Select[ select <: t.Select ] extends Builder[select] {
    def select
      ( implicit parser: ResultParser[ driver, Iterable[ member.Value with api.Persisted ] ] )
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
      new Builder(member, template, values) with Select[ select ]
    }
  }

}
