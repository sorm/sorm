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

import Query._

import collection.immutable.Queue

class Fetcher
  [ T ]
  ( connection      : ConnectionAdapter,
    queryMapping    : EntityMapping,
    queryWhere      : Option[Where] = None,
    queryOrder      : Queue[Order] = Queue.empty,
    queryLimit      : Option[Int] = None,
    queryOffset     : Int = 0 )
  {
    private def copy
      ( connection      : ConnectionAdapter = connection,
        queryMapping    : EntityMapping = queryMapping,
        queryWhere      : Option[Where] = queryWhere,
        queryOrder      : Queue[Order] = queryOrder,
        queryLimit      : Option[Int] = queryLimit,
        queryOffset     : Int = queryOffset )
      : Fetcher[T]
      = new Fetcher[T](
          connection, queryMapping, queryWhere, queryOrder, queryLimit, queryOffset
        )

    def filter ( w : Where )
      = copy(
          queryWhere = (queryWhere ++: List(w)) reduceOption And
        )
    def order ( p : String, r : Boolean = false )
      = copy( queryOrder = queryOrder enqueue Order(MappingPath(queryMapping, p), r) )
    def limit ( x : Int )
      = copy( queryLimit = Some(x) )
    def offset ( x : Int )
      = copy( queryOffset = x )

    def filterEquals ( p : String, v : Any )
      = filter( WherePath.where( queryMapping, p, v, Operator.Equals ) )
    def filterNotEquals ( p : String, v : Any )
      = filter( WherePath.where( queryMapping, p, v, Operator.NotEquals ) )
    def filterContains ( p : String, v : Any )
      = filter( WherePath.where( queryMapping, p, v, Operator.Contains ) )
    def filterIn ( p : String, v : Any )
      = filter( WherePath.where( queryMapping, p, v, Operator.In ) )

    private def query( kind : Kind )
      = Query(kind, queryMapping, queryWhere, queryOrder, queryLimit, queryOffset)

    def fetchAll()
      : Seq[T with Persisted]
      = {
        val (stmt, resultSetMappings)
          = query(Kind.Select).statementAndResultMappings

        connection.executeQuery(stmt)
          .fetchInstancesAndClose(
            queryMapping,
            resultSetMappings.view.zipWithIndex.toMap
          )
          .asInstanceOf[Seq[T with Persisted]]
      }

    def fetchOne()
      = limit(1).fetchAll().headOption

    def fetchSize()
      : Int
      = {
        val (stmt, _)
          = query(Kind.Count).statementAndResultMappings

        connection.executeQuery(stmt)
          .parseAndClose()
          .head.head
          .asInstanceOf[Int]
      }

  }
