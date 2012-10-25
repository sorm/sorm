package sorm.abstractSql

import sext._, embrace._

import AbstractSql._

object Compositing {

  def intersection 
    ( left : Statement, right : Statement ) 
    : Statement
    = (left, right) match {
        case (l : Select, r : Select)
          if l.expressions == r.expressions &&
             havingsMergeable(l.having, r.having) =>
          l.copy(
            condition
              = ( l.condition ++ r.condition ) reduceOption And,
            having
              = l.having ++: r.having distinct,
            limit
              = (l.limit, r.limit) match {
                  case (Some(l), Some(r)) => Seq(l, r).min $ (Some(_))
                  case (l, r) => l orElse r
                },
            offset
              = Seq(l.offset, r.offset).max
          )
        case (l, r) =>
          Intersection(l, r)
      }
  
  def union 
    ( left : Statement, right : Statement ) 
    : Statement
    = (left, right) match {
        case (l : Select, r : Select)
          if l.expressions == r.expressions &&
             havingsMergeable(l.having, r.having) =>
          l.copy(
            condition
              = ( l.condition ++ r.condition ) reduceOption Or,
            having
              = l.having ++: r.having distinct,
            limit
              = (l.limit, r.limit) match {
                  case (Some(l), Some(r)) => Seq(l, r).max $ (Some(_))
                  case (l, r) => l orElse r
                },
            offset
              = Seq(l.offset, r.offset).min
          )
        case (l, r) =>
          Union(l, r)
      }
  
  private def havingsMergeable
    ( l : Seq[HavingCount], r : Seq[HavingCount] )
    = l.forall{ l =>
        !r.exists{ r =>
          l.column == r.column &&
          l.table == r.table &&
          l != r
        }
      }

}