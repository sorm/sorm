package vorm

import vorm._
import extensions._
import structure._
import query._
import selectAbstraction._

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



}