package vorm

import vorm._
import extensions._
import structure._
import query._
import selectAbstraction._
import vorm.{sql => Sql}

package object pkSelect {


  def select
    ( c : Clause.Select )
    : Sql.Select
    = {

      def selectWithClause
        ( s : Sql.Select, c : Clause )
        : Sql.Select
        = ???


      c.mapping.primaryKeySelect
        .foldFrom(c.rows) {
          (s, r)
            ⇒ s.copy(
                  having
                    = Sql.Clause.Equals(
                          Sql.Count(
                              s.what.asInstanceOf[Seq[Sql.Column]],
                              true
                            ),
                          r
                        ),
                  groupBy
                    = s.what.asInstanceOf[Seq[Sql.Column]]
                )
        }
        .foldFrom(c.clause)(selectWithClause)
    }



}