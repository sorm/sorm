package vorm

import vorm._
import extensions._
import structure._
import query._
import selectAbstraction._
import vorm.{sql => Sql}

package object pkSelect {

  def clause
    ( n : Query.Where )
    : Clause
    = n match {
        case Query.Where.Contains( m : mapping.Seq, v )
          ⇒ clause( Query.Where.Includes(m, Seq(v)) )
        case Query.Where.Includes( m : mapping.Seq, v : Seq[_] )
          ⇒ Clause.Select(
                 mapping
                   = m,
                 clause
                   = v.view
                       .map( Query.Where.Equals(m.child, _) )
                       .map( clause )
                       .reduceOption( Clause.Or ),
                 rows
                   = Some( v.length )
               )
        case Query.Where.Equals( m : mapping.Seq, v : Seq[_] )
          ⇒ Clause.Select(
                mapping 
                  = m,
                clause
                  = v.view
                      .map( Query.Where.Equals(m.child, _) )
                      .map( clause )
                      .reduceOption( Clause.Or )
                      .foldRight( 
                          clause( Query.Where.HasSize( m, v.length ) )
                        )( Clause.And )
                      .some,
                rows 
                  = Some( v.length )
              )
        case Query.Where.HasSize( m : mapping.Table, v : Int )
          ⇒ Clause.Select( m, None, Some(v) )
        case Query.Where.Equals( m : mapping.Value, v )
          ⇒ Clause.Equals( m, v )

      }




  def select
    ( c : Clause.Select )
    : Sql.Select
    = {

      def selectWithClause
        ( s : Sql.Select, c : Clause )
        : Sql.Select
        = ???


      c.mapping.selectSkeleton.copy(
          what 
            = c.mapping.primaryKeyColumns.view
                .map( _.name )
                .map( Sql.Column(_, Some(c.mapping.skeletonTablesAliases(c.mapping))) )
                .toList
        )
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