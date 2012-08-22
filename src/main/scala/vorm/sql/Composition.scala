package vorm.sql

import vorm._
import query.Query
import structure._
import mapping._
import extensions._
import Sql._

object Composition {

  def alias ( x : Int ) = ( 97 + x ).toChar.toString
  
  implicit class SelectExtensions
    ( self : Select )
    {
      def narrow
        ( other : Union )
        : Select
        = {
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
      /**
       * The `other` Select is required to have only columns and only those of
       * the `from` table in the `what` clause.
       */
      def narrow
        ( other : Select )
        : Select
        = if( self.from == other.from )
            if( self.join == other.join &&
                self.orderBy == other.orderBy &&
                self.groupBy == other.groupBy &&
                self.limit == other.limit &&
                self.offset == other.offset &&
                ( self.having.isEmpty || other.having.isEmpty ) )
              self.copy(
                where
                  = ( self.where ++: other.where ++: Stream() )
                      .reduceOption{ CompositeCondition(_, _, And) },
                having
                  = ( self.having ++: other.having ++: Stream() )
                      .reduceOption{ CompositeCondition(_, _, And) }
              )
            else {
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
          else
            ???
    }


  implicit class StatementExtensions 
    ( self : Statement )
    {
      def what : Seq[WhatObject]
        = self match {
            case self : Union => self.left.what
            case self : Select => self.what
          }
      def narrow ( other : Statement )
        = Select(self) narrow Select(other)
      def widen ( other : Statement )
        = (self, other) match {
            case (self : Select, other : Select) =>
              self widen other
            case _ =>
              Union(self, other)
          }
    }

  object Select {
    def apply
      ( s : Statement )
      : Select
      = s match {
          case s : Select => s
          case _ => ???
        }
    def apply
      ( query : Query.Query )
      : Select
      = resultSet(query.mapping)
          .narrow(
            query.where
              .map{ Statement.primaryKey(query.mapping, _) }
              .map{ apply }
              .getOrElse( Select.primaryKey(query.mapping) )
              .copy(
                //  todo: to add order
                limit = query.limit,
                offset = Some(query.offset)
              )
          )



    def primaryKey
      ( mapping : TableMapping )
      : Select
      = ???
    def resultSet
      ( mapping : TableMapping )
      : Select
      = {
        ???
      }
  }

  object Statement {
    // def apply
    //   ( query : Query.Query )
    //   : Statement
    //   = Select(
    //       from
    //         = From(Table(query.mapping.tableName), Some(Sql.alias(0))),
    //       // what
    //       //   = ???,
    //       // join
    //       //   = ???,

    //     )

    def countItems
      ( mapping : CollectionMapping )
      : Statement
      = ???

    def primaryKey
      ( mapping : TableMapping,
        where : Query.Where )
      : Statement
      = where match {
          case Query.And(l, r) => 
            primaryKey(mapping, l) narrow primaryKey(mapping, r)
          case Query.Or(l, r) => 
            primaryKey(mapping, l) widen primaryKey(mapping, r)
          case Query.Filter( Query.Operator.Equals, 
                             m : SeqMapping, 
                             v : Seq[_] ) =>
            Select.primaryKey(mapping)

        } 
  }

}