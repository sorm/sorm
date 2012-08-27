package sorm.abstractSql

import sorm._
import structure._
import mapping._
import extensions.Extensions._

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
              = l.having ++: r.having distinct
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
              = l.having ++: r.having distinct
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