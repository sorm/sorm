package vorm.abstractSql

import vorm._
import structure._
import mapping._

import AbstractSql._

object Composition {

  implicit class StatementCompositionOperations
    ( self : Statement )
    {
      def intersect
        ( other : Statement )
        : Statement
        = ???
      def union
        ( other : Statement )
        : Statement
        = ???
      def toSelect
        : Select
        = ???
    }
}