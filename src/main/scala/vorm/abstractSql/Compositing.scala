package vorm.abstractSql

import vorm._
import structure._
import mapping._
import extensions._

import AbstractSql._

object Compositing {

  def intersection 
    ( left : Statement, right : Statement ) 
    : Statement
    = (left, right) match {
        case (l : Select, r : Select)
          if l.expressions == r.expressions &&
             ( l.havingCount.isEmpty || r.havingCount.isEmpty ) => 
          l.copy(
            condition
              = ( l.condition ++ r.condition ) reduceOption And,
            havingCount
              = l.havingCount orElse r.havingCount
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
             ( l.havingCount.isEmpty || r.havingCount.isEmpty ) => 
          l.copy(
            condition
              = ( l.condition ++ r.condition ) reduceOption Or,
            havingCount
              = l.havingCount orElse r.havingCount
          )
        case (l, r) =>
          Union(l, r)
      }
  
  def select 
    ( statement : Statement ) 
    : Select
    = statement match {
        case statement : Select => statement
      }

}