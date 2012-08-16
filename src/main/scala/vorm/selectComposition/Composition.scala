package vorm.selectComposition

import vorm._
import persisted._
import structure._
import query.Query._
import mapping._
import sql._
import ddl._
import extensions._

object Composition {

  implicit class ExecutableExtensions 
    ( self : Executable )
    {
      def intersect ( other : Executable )
        = (self, other) match {
            case ( self : Select, other : Select ) => self narrow other
            case _ => ???
          }
      def union ( other : Executable )
        = Union( self, other )
    }

  object Select {
    def apply
      ( query : Query )
      : Select
      = ???

    def countItems
      ( mapping : CollectionMapping )
      : Select
      = ???
  }


}