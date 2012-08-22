package vorm.sql

import vorm._
import query.Query
import structure._
import mapping._
import extensions._
import Sql._

object Composition {

  def alias ( x : Int ) = ( 97 + x ).toChar.toString
  
  implicit class StatementOperations 
    ( self : Statement )
    {
      def what : Seq[WhatObject]
        = self match {
            case self : Union => self.left.what
          }
      /**
       * The `other` Select is required to have only columns and only those of
       * the `from` table in the `what` clause.
       */
      def narrow ( other : Statement ) : Select
        = (self, other) match {
            case (self : Union, other : Select) =>
              other narrow self
            case (self : Union, _) =>
              self.toSelect narrow other
            case (self : Select, other : Select) 
              if self.from == other.from &&
                 self.join == other.join &&
                 self.orderBy == other.orderBy &&
                 self.groupBy == other.groupBy &&
                 self.limit == other.limit &&
                 self.offset == other.offset &&
                 ( self.having.isEmpty || other.having.isEmpty )
              =>
              self.copy(
                where
                  = ( self.where ++: other.where ++: Stream() )
                      .reduceOption{ CompositeCondition(_, _, And) },
                having
                  = ( self.having ++: other.having ++: Stream() )
                      .reduceOption{ CompositeCondition(_, _, And) }
              )
            case (self : Select, _) =>
              val newAlias = alias( self.join.size + 1 )
              self.copy(
                join
                  = self.join :+
                    Join(
                      what = other,
                      as = Some( newAlias ),
                      on = other.what.asInstanceOf[Seq[Column]]
                             .view
                             .map{_.name}
                             .map{n => 
                               Column(n, Some(newAlias)) ->
                               Column(n, self.from.as)
                             },
                      kind = JoinKind.Right
                    )
              )
          } 
      def widen ( other : Statement ) : Statement
        = (self, other) match {
            case (self : Select, other : Select) =>
              ???
            case _ =>
              Union(self, other)
          }
      def toSelect : Select
        = self match {
            case self : Union =>
            case self : Select => self
          }
    }


}