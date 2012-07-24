package vorm

import extensions._
import query._
import structure._
import reflection._

package object select {


  /**
   * left and right must select the same columns
   */
  def intersect
    ( left : Select,
      right : Select )
    : Select
    = if( left.having == Nil || right.having == Nil )
        left.copy(
            where 
              = ( left.where, right.where ) match {
                  case ( Some(l), Some(r) ) 
                    ⇒ Some( Where.And(l, r) )
                  case ( _, _ ) 
                    ⇒ ( left.where +: right.where +: Nil ).headOption
                },
            having 
              = left.having ++ right.having
          )
      else 
        left.copy(
            joins
              = Join(
                    what = right,
                    as = "t" + (left.joins.size + 1),
                    to = left.from.alias.getOrElse(left.from.name),
                    on = left.columns.map(c ⇒ c.name → c.name),
                    kind = JoinKind.Inner
                  ) +:
                left.joins
          )

  /**
   * left and right must select the same columns
   */
  def union
    ( left : Select,
      right : Select )
    : Select
    = if( left.having == Nil || right.having == Nil )
        left.copy(
            where 
              = ( left.where, right.where ) match {
                  case ( Some(l), Some(r) ) 
                    ⇒ Some( Where.Or(l, r) )
                  case ( _, _ ) 
                    ⇒ ( left.where +: right.where +: Nil ).headOption
                },
            having 
              = left.having ++ right.having
          )
      else 
        left.copy(
            joins
              = Join(
                    what = right,
                    as = "t" + (left.joins.size + 1),
                    to = left.from.alias.getOrElse(left.from.name),
                    on = left.columns.map(c ⇒ c.name → c.name),
                    kind = JoinKind.Outer
                  ) +:
                left.joins
          )


    


}