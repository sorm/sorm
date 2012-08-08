package vorm.api

import vorm._
import persisted._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import query._
import select._
import resultSet._
import extensions._

case class QueryAdapter
  [ T ]
  ( connection : ConnectionAdapter,
    mapping    : EntityMapping,
    where      : Option[Query.Where] = None,
    order      : Seq[Query.Order] = Nil,
    limit      : Option[Int] = None,
    offset     : Int = 0 )
  extends Iterable[T]
  {
    def filter ( w : Query.Where )
      = copy(
          where = (where ++: List(w)) reduceOption Query.Where.And
        )

    private def resolveMapping ( p : String ) : Mapping
      = p.satisfying{ !_.contains(".") }
          .map{ mapping.properties }
          .getOrElse{ throw new Exception("Complex paths are not supported yet") }

    def filterEquals ( p : String, v : Any )
      = filter( Query.Where.Equals( resolveMapping(p), v ) )

  // def order(prop: String, reverse: Boolean = false) =
  //   new QueryStream[T](
  //     connection,
  //     query.copy(orderings = Ordering(prop, reverse) :: query.orderings)
  //   )

  // def offset(offset: Int) =
  //   new QueryStream[T](
  //     connection,
  //     query.copy(limit = query.limit.copy(offset = offset))
  //   )

  // def limit(amount: Int) =
  //   new QueryStream[T](
  //     connection,
  //     query.copy(limit = query.limit.copy(amount = Some(amount)))
  //   )
  

    private def query( kind : Query.Kind )
      = Query(kind, mapping, where, order, limit, offset)

    override def toSeq
      = {
        val (stmt, resultSetMappings) 
          = query(Query.Kind.Select).statementAndResultMappings

        connection.executeQuery(stmt)
          .fetchInstancesAndClose( 
            mapping, 
            resultSetMappings.view.zipWithIndex.toMap
          )
          .asInstanceOf[Seq[T]]
      }
    override def toList 
      = toSeq.toList
    def iterator 
      = toSeq.iterator

    /**
     * Emits a count query every time it's called
     */
    override def size : Int 
      = {
        val (stmt, _) 
          = query(Query.Kind.Count).statementAndResultMappings

        connection.executeQuery(stmt)
          .parseAndClose()
          .head.head
          .asInstanceOf[Int]
      }
  }
