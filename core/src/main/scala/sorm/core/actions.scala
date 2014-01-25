package sorm.core.actions

import sorm._, core._, util._
import core.static._


case class Action[ result ]( ast: AST )

trait Runner {
  def run[ result ]( action: Action[ result ] ): result = ???
  def runAsTransaction[ result ]( action: Action[ result ] ): result = ???
}

sealed trait AST
object AST {
  case class From
    [ root ]
    extends AST
  case class Limit
    [ tail ]
    ( limit: Int, tail: tail )
    extends AST
  case class Offset
    [ tail ]
    ( offset: Int, tail: tail )
    extends AST
  case class Where
    [ tail ]
    ( tail: tail )
    extends AST
  case class Select
    [ tail ]
    ( tail: tail )
    extends AST
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
    type AST <: actions.AST
    type Root
    protected val ast: AST
  }
  object FromBuilder {
    implicit def selectSupport( b: FromBuilder{ type SelectSupport = True } ) = new {
      def select = Action[b.Root](b.ast)
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
        type AST = AST.Limit[b.AST]
        type Root = b.Root
        protected val ast = AST.Limit(a, b.ast)
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
        type AST = AST.Offset[b.AST]
        type Root = b.Root
        protected val ast = AST.Offset(a, b.ast)
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
      type AST = AST.From[a]
      type Root = a
      protected val ast = AST.From[a]
    }
  def insert[ a ](a: a) = ???



}
