package sorm.core.actions

import sorm._, core._, util._
import core.static._

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
  implicit class RunOps[r](val self: Action[r]) extends AnyVal {
    def runOn( runner: ActionRunner ) = runner.run(self)
    def runAsTransactionOn( runner: ActionRunner ) = runner.runAsTransaction(self)
  }
}

trait ActionRunner {
  def run[ result ]( action: Action[ result ] ): result = ???
  def runAsTransaction[ result ]( action: Action[ result ] ): result = ???
}

trait StatementRunner {
  def run[ result ]( statement: syntax.Statement[ result ] ): result
}

object syntax {
  case class From[ root ]
  case class Limit[ tail ]( limit: Int, tail: tail )
  case class Offset[ tail ]( offset: Int, tail: tail )
  case class Where[ tail ]( tail: tail )

  sealed trait Statement[ result ]
  object Statement {
    case class Select[ result, tail ]( tail: tail ) extends Statement[ result ]
  }
}

trait Builders {

  import typeLevel._, Bool._

  sealed trait FromBuilder {
    type SelectSupport <: Bool
    type UpdateSupport <: Bool
    type DeleteSupport <: Bool
    type WhereSupport <: Bool
    type OrderBySupport <: Bool
    type LimitSupport <: Bool
    type OffsetSupport <: Bool
    type AST
    type Root
    protected val ast: AST
  }
  object FromBuilder {
    implicit def selectSupport( b: FromBuilder{ type SelectSupport = True } ) = new {
      def select = {
        val statement = syntax.Statement.Select[b.Root, b.AST](b.ast)
        Action(runner => runner.run(statement))
      }
    }
    implicit def limitSupport( b: FromBuilder{ type LimitSupport = True } ) = new {
      def limit(a: Int) = new FromBuilder {
        type SelectSupport = b.SelectSupport
        type UpdateSupport = b.UpdateSupport
        type DeleteSupport = b.DeleteSupport
        type WhereSupport = b.WhereSupport
        type OrderBySupport = b.OrderBySupport
        type LimitSupport = False
        type OffsetSupport = b.OffsetSupport
        type AST = syntax.Limit[b.AST]
        type Root = b.Root
        protected val ast = syntax.Limit(a, b.ast)
      }
    }
    implicit def offsetSupport( b: FromBuilder{ type OffsetSupport = True } ) = new {
      def offset(a: Int) = new FromBuilder {
        type SelectSupport = b.SelectSupport
        type UpdateSupport = b.UpdateSupport
        type DeleteSupport = b.DeleteSupport
        type WhereSupport = b.WhereSupport
        type OrderBySupport = b.OrderBySupport
        type LimitSupport = b.LimitSupport
        type OffsetSupport = False
        type AST = syntax.Offset[b.AST]
        type Root = b.Root
        protected val ast = syntax.Offset(a, b.ast)
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
      type AST = syntax.From[a]
      type Root = a
      protected val ast = syntax.From[a]
    }
  def insert[ a ](a: a) = ???



}
