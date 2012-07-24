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
              = (left.where ++ right.where) reduceOption Clause.And,
            having 
              = (left.having ++ right.having) reduceOption Clause.And
          )
      else 
        left.copy(
            joins
              = Join(
                    what = right,
                    as = Some("t" + (left.joins.size + 1)),
                    to = left.from.as.get,
                    on = left.what.asInstanceOf[Seq[Column]]
                                  .map(c ⇒ c.name → c.name),
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
              = (left.where ++ right.where) reduceOption Clause.Or,
            having 
              = (left.having ++ right.having) reduceOption Clause.Or
          )
      else 
        left.copy(
            joins
              = Join(
                    what = right,
                    as = Some("t" + (left.joins.size + 1)),
                    to = left.from.as.get,
                    on = left.what.asInstanceOf[Seq[Column]]
                                  .map(c ⇒ c.name → c.name),
                    kind = JoinKind.Outer
                  ) +:
                left.joins
          )


    


}