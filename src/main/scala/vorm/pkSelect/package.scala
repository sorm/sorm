package vorm

import vorm._
import reflection._
import structure._
import query._
import select._

package object pkSelect {

  def pkSelect
    ( q : Query )
    : Select
    = {
      val m = q.mapping

      def select
        ( n : Query.Where )
        : Select
        = {
          val alias
            : mapping.Table ⇒ String
            = new collection.mutable.HashMap[mapping.Table, String]() {
                override def default
                  ( m : mapping.Table )
                  = {
                    val n = "t" + size
                    update(m, n)
                    n
                  }
              }

          def basis
            = Select(
                  // groupBy = GroupBy(n.mapping.root.primaryKey)
                )
          n match {
            case Query.Where.And( left, right )
              ⇒ intersect( select(left), select(right) )
            case Query.Where.Or( left, right )
              ⇒ union( select(left), select(right) )
            case Query.Where.Includes( m : mapping.Seq, v : Seq[_] )
              ⇒ val s
                  = select(
                        v.view
                          .map(Query.Where.Equals(m.item, _))
                          .reduce(Query.Where.Or)
                      )
                s.copy(
                    having 
                      = s.having ++:
                        Condition( 
                            Left(
                                Count(
                                    m.primaryKeyColumns.view
                                      .map( _.name )
                                      .map( Column( _, Some( alias(m) ) ) ),
                                    true
                                  )
                              ),
                            ConditionRelation.Equal,
                            Right(v.size)
                          ) ::
                        Nil reduceOption Clause.And
                          // .foldTo(s.having)(Clause.And)
                  )

            // case Query.WhereNode.Equals( m : mapping.Value, v )
            //   ⇒ sql.WhereNode.Equals(
            //       alias(m.parentTableMapping) + "." + m.column.name,
            //       (v, m.column.t.jdbcType)
            //     )
            // case Query.WhereNode.Equals( m : mapping.Tuple, v : Product ) 
            //   ⇒ m.children
            //       .view
            //       .map( m ⇒ m.child -> m.index )
            //       .map{ 
            //         case (m, i) 
            //           ⇒ Query.WhereNode.Equals( m, v.productElement(i) ) 
            //       }
            //       .map( where )
            //       .reduce( sql.WhereNode.And )
            // case Query.WhereNode.Equals( m : mapping.Option, v : Option[_] )
            //   ⇒ v match {
            //       case Some(v) 
            //         ⇒ where( Query.WhereNode.Equals(m.child.child, v) )
            //       case None
            //         ⇒ where( Query.WhereNode.Equals(m.child.child, null) )
            //     }
          }
        }
    }


}