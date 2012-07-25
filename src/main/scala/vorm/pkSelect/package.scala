package vorm

import vorm._
import extensions._
import reflection._
import structure._
import query._
import select._

package object pkSelect {

  def selectionNode
    ( n : Query.Where )
    : SelectionNode
    = n match {
        case Query.Where.Contains( m : mapping.Seq, v )
          ⇒ selectionNode( Query.Where.Includes(m, Seq(v)) )
        case Query.Where.Includes( m : mapping.Seq, v : Seq[_] )
          ⇒ SelectionNode.Select(
                task 
                  = SelectionNode.Task.PrimaryKey,
                mapping 
                  = m,
                clause
                  = v.view
                      .map( Query.Where.Equals(m.child, _) )
                      .map( selectionNode )
                      .reduceOption( SelectionNode.Or ),
                rows 
                  = Some( v.size )
              )
        case Query.Where.Equals( m : mapping.Seq, v : Seq[_] )
          ⇒ SelectionNode.Select(
                task 
                  = SelectionNode.Task.PrimaryKey,
                mapping 
                  = m,
                clause
                  = v.view
                      .map( Query.Where.Equals(m.child, _) )
                      .map( selectionNode )
                      .reduceOption( SelectionNode.Or )
                      .foldRight( 
                          selectionNode( Query.Where.HasSize( m, v.size ) )
                        )( SelectionNode.And )
                      .some,
                rows 
                  = Some( v.size )
              )
        case Query.Where.HasSize( m : mapping.Table, v : Int )
          ⇒ SelectionNode.Select(
                task 
                  = SelectionNode.Task.PrimaryKey,
                mapping 
                  = m,
                rows 
                  = Some( v )
              )
        case Query.Where.Equals( m : mapping.Value, v )
          ⇒ SelectionNode.Equals( m, v )

      }



}