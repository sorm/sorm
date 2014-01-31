package sorm.core.expressions.builders

import sorm._, core._, util._, expressions._
import expressions.{templates => t}
import expressions.{values => v}

trait API extends members.API { thisAPI =>

  protected val engine: core.engine.Engine

  protected sealed trait FromBuilder {
    protected val member: core.members.Member
    protected val template: t.SelectSpec
    protected val values: v.SelectSpec
  }
  protected object FromBuilder {

    sealed trait Select extends FromBuilder {
      type result = Iterable[ member.Value with api.Persisted ]
      def select
        ( implicit parser: engine.parser[ result ],
                   compiler: engine.compiler[ t.Action.Select[ t.SelectSpec ], v.Action.Select[ v.SelectSpec ] ] )
        = {
          val actionTemplate = t.Action.Select(this.template)
          val actionValues = v.Action.Select(this.values)
          engine.runExpression(actionTemplate, actionValues, compiler, parser, member)
        }
    }

    sealed trait Limit extends FromBuilder { thisLimit =>
      def limit( a: Int ) =
        new FromBuilder with Select {
          protected val member = thisLimit.member
          protected val template = t.SelectSpec.Limit(thisLimit.template)
          protected val values = v.SelectSpec.Limit(a, thisLimit.values)
        }
    }

  }

  def from[ a ]
    ( implicit memberResolver: MemberResolver[ a ] )
    =
    new FromBuilder with FromBuilder.Select with FromBuilder.Limit {
      protected val member = memberResolver.apply
      protected val template = t.SelectSpec.From()
      protected val values = v.SelectSpec.From()
    }
}
