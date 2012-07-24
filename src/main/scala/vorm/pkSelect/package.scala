package vorm

import vorm._
import reflection._
import structure._

package object pkSelect {



  def select
    ( n : Query.WhereNode )
    : Select
    = {
      def basis
        = Select(
              // groupBy = GroupBy(n.mapping.root.primaryKey)
            )
      n match {
        case Query.WhereNode.And( left, right )
          ⇒ intersect( select(left), select(right) )
        case Query.WhereNode.Or( left, right )
          ⇒ union( select(left), select(right) )
        case Query.WhereNode.Includes( m : mapping.Seq, v : Seq[_] )
          ⇒ val s
              = select(
                    v.view
                      .map(Query.WhereNode.Equals(m.item, _))
                      .reduce(Query.WhereNode.Or)
                  )
            s.copy(
                having 
                  = Having( Count(distinct = true, m.primaryKey) )
              )
      }
    }



}