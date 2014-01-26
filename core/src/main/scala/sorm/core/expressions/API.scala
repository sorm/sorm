package sorm.core.expressions

import sorm._, core._, util._
import typeLevel.Bool, Bool._

trait API {

  case class Action[ result ]( f: StatementRunner => result )
  object Action extends scalaz.syntax.ToMonadOps {
    implicit val monad = new scalaz.Monad[Action] {
      def point[a](a: => a) = Action(_ => a)
      def bind[a, b](a: Action[a])(k: a => Action[b]) = {
        Action{ runner =>
          val aResult = a.f(runner)
          val bAction = k(aResult)
          bAction.f(runner)
        }
      }
    }
    implicit class RunOps[r](self: Action[r]) {
      def runOn( runner: ActionRunner ) = runner.run(self)
      def runAsTransactionOn( runner: ActionRunner ) = runner.runAsTransaction(self)
    }
  }

  trait ActionRunner {
    def run[ result ]( action: Action[ result ] ): result
    def runAsTransaction[ result ]( action: Action[ result ] ): result
  }

  trait StatementRunner {
    def run[ result ]( template: templates.Statement, values: Seq[Any] ): result
  }

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
    implicit def selectOps( b: FromBuilder{ type SelectSupport = True } ) = new {
      def select = {
        val template = templates.Statement.Select(b.template)
        val values = b.values
        Action(runner => runner.run(template, values))
      }
    }
    implicit def limitOps( b: FromBuilder{ type LimitSupport = True } ) = new {
      def limit(a: Int) = new FromBuilder {
        type SelectSupport = b.SelectSupport
        type UpdateSupport = b.UpdateSupport
        type DeleteSupport = b.DeleteSupport
        type WhereSupport = b.WhereSupport
        type OrderBySupport = b.OrderBySupport
        type LimitSupport = False
        type OffsetSupport = b.OffsetSupport
        type Template = templates.Limit[b.Template]
        protected val template = templates.Limit(b.template)
        protected val values = a +: b.values
      }
    }
    implicit def offsetOps( b: FromBuilder{ type OffsetSupport = True } ) = new {
      def offset(a: Int) = new FromBuilder {
        type SelectSupport = b.SelectSupport
        type UpdateSupport = b.UpdateSupport
        type DeleteSupport = b.DeleteSupport
        type WhereSupport = b.WhereSupport
        type OrderBySupport = b.OrderBySupport
        type LimitSupport = b.LimitSupport
        type OffsetSupport = False
        type Template = templates.Offset[b.Template]
        protected val template = templates.Offset(b.template)
        protected val values = a +: b.values
      }
    }
  }

  def from[ a ] =
    new FromBuilder {
      type SelectSupport = True
      type UpdateSupport = True
      type DeleteSupport = True
      type WhereSupport = True
      type OrderBySupport = True
      type LimitSupport = True
      type OffsetSupport = True
      type Template = templates.From[a]
      protected val template = templates.From[a]
      protected val values = Nil
    }
  def insert[ a ](a: a) = ???



}
