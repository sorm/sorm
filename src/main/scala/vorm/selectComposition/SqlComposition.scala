package vorm.selectComposition

import vorm._
import persisted._
import structure._
import query._
// import Query._
import mapping._
import sql._
import extensions._

object SqlComposition {

  implicit class ExecutableExtensions 
    ( self : Executable )
    {
      /**
       * The `other` Select is required to have only columns and only those of
       * the `from` table in the `what` clause.
       */
      def narrow ( other : Executable )
        = (self, other) match {
            // case ( self : Select, other : Select ) => self narrow other
            case ( self : Select, other : Select ) => 
              if( self.from == other.from )
                if( self.join == other.join &&
                    self.orderBy == other.orderBy &&
                    self.groupBy == other.groupBy &&
                    self.limit == other.limit &&
                    self.offset == other.offset )
                  if( self.where == other.where &&
                      self.having == other.having )
                    self
                  else
                    self.copy(
                      where
                        = ( self.where ++: other.where ++: Stream() )
                            .reduceOption{ Clause.And },
                      having
                        = ( self.having ++: other.having ++: Stream() )
                            .reduceOption{ Clause.And }
                    )
                else {
                  lazy val newAlias = alias( self.join.size + 1 )
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
            case _ => ???
          }
      def widen ( other : Executable )
        = Union( self, other )
    }

  object Executable {
    def resultSetSelect
      ( mapping : TableMapping )
      : Select
      = {
        ???
      }
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

    def primaryKey
      ( mapping : Mapping,
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
            ???
        }

    def countItems
      ( mapping : CollectionMapping )
      : Executable
      = ???
  }

}