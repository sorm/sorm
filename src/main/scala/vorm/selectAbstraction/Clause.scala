package vorm.selectAbstraction

import vorm._
import extensions._
import query._
import structure._
import reflection._


trait Clause
object Clause {

  trait Composite extends Clause {
    def left : Clause
    def right : Clause
  }

  case class And
    ( left : Clause,
      right : Clause )
    extends Composite

  case class Or
    ( left : Clause,
      right : Clause )
    extends Composite

  trait Filter extends Clause {
    def mapping : Mapping
  }

  case class Select
    ( mapping : structure.mapping.Table,
      clause : Option[Clause] = None,
      rows : Option[Int] = None )
    extends Filter

  case class Equals
    ( mapping : structure.mapping.Value,
      value : Any )
    extends Filter


  def apply
    ( w : Query.Where )
    : Clause
    = w match {
        case Query.Where.Contains( m : mapping.Seq, v )
          ⇒ apply( Query.Where.Includes(m, Seq(v)) )
        case Query.Where.Includes( m : mapping.Seq, v : Seq[_] )
          ⇒ Clause.Select(
                 mapping
                   = m,
                 clause
                   = v.view
                       .map( Query.Where.Equals(m.child, _) )
                       .map( apply )
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
                      .map( apply )
                      .reduceOption( Clause.Or )
                      .foldRight( 
                          apply( Query.Where.HasSize( m, v.length ) )
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
