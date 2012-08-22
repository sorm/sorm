package vorm.selectComposition

import vorm._
import persisted._
import structure._
import query._
import mapping._
import sql._
import Sql._
import extensions._

object SqlComposition {

  implicit class SelectExtensions
    ( self : Select )
    {
      def narrow 
        ( other : Executable ) 
        : Select
        = narrow(Select(other))
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
                      .reduceOption{ Clause.And },
                having
                  = ( self.having ++: other.having ++: Stream() )
                      .reduceOption{ Clause.And }
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

  implicit class ExecutableExtensions 
    ( self : Executable )
    {
      def narrow
        ( other : Executable )
        : Executable
        = Select(self) narrow other
      def widen 
        ( other : Executable )
        : Executable
        = Union(self, other)
    }

  object Select {
    def apply
      ( executable : Executable )
      : Select
      = executable match {
          case executable : Select => executable
          case _ => ???
        }
    def apply
      ( query : Query.Query )
      : Select
      = resultSet(query.mapping)
          .narrow(
            query.where
              .map{ Executable.primaryKey(query.mapping, _) }
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

  object Executable {
    // def apply
    //   ( query : Query.Query )
    //   : Executable
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
      : Executable
      = ???

    def primaryKey
      ( mapping : TableMapping,
        where : Query.Where )
      : Executable
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