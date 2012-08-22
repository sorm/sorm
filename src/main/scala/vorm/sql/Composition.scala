package vorm.sql

import vorm._
import extensions._
import Sql._

object Composition {

  def alias ( x : Int ) = ( 97 + x ).toChar.toString
  
  implicit class StatementCompositionOperations
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
            case self : Union => ???
            case self : Select => self
          }
      /**
       * Drops orphan joins. Not tested at all
       */
      def optimized : Statement
        = self match {
            case Union(l, r) => Union(l.optimized, r.optimized)
            case self : Select =>
              val refs
                : Set[String]
                = {
                  val whatRefs
                    = self.what.view collect { case Column(_, Some(r)) ⇒ r }
                  val fromRef
                    = self.from.as
                  val whereRefs
                    = ???
                  val groupByRefs
                    = self.groupBy collect { case Column(_, Some(r)) ⇒ r }
                  val havingRefs
                    = ???

                  Set() ++ whatRefs ++ fromRef ++ whereRefs ++ groupByRefs ++ havingRefs
                }

              def f
                ( s : Select )
                : Select
                = {
                  val joinRefs
                    = s.join.view flatMap {
                        _.on collect { case (_, Column(_, Some(r))) ⇒ r }
                      }

                  val allRefs
                    = refs ++ joinRefs

                  val filtered
                    = s.join filter {
                        _.as map { allRefs contains _ } getOrElse false
                      }

                  if( filtered == s.join )
                    s
                  else
                    f( s copy ( join = filtered ) )
                }

              def withSubSelectsOptimized
                ( s : Select )
                = s.copy(
                      join
                        = s.join map { j ⇒
                            j.what match {
                              case s : Select ⇒ j.copy(s.optimized)
                              case _ ⇒ j
                            }
                          }
                    )

              withSubSelectsOptimized( f(self) )

        }
    }


}