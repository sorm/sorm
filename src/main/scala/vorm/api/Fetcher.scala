package vorm.api

import vorm._
import abstractSql.StandardSqlComposition
import query.AbstractSqlComposition
import sql.StandardRendering._
import persisted._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import resultSet._
import extensions._

import query.Query._

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

    def order ( p : String, r : Boolean = false )
      = copy( queryOrder = queryOrder enqueue Order(Path.mapping(queryMapping, p), r) )
    def limit ( x : Int )
      = copy( queryLimit = Some(x) )
    def offset ( x : Int )
      = copy( queryOffset = x )

    def filter ( w : Where )
      : Fetcher[T]
      = copy( 
          queryWhere = (queryWhere ++: List(w)) reduceOption And
        )
    private def filter ( p : String, v : Any, o : Operator )
      : Fetcher[T]
      = filter( Path.where( queryMapping, p, v, o ) )

    def filterEquals ( p : String, v : Any ) 
      = filter( p, v, Operator.Equals )
    def filterNotEquals ( p : String, v : Any ) 
      = filter( p, v, Operator.NotEquals )
    def filterSmaller ( p : String, v : Any ) 
      = filter( p, v, Operator.Smaller )
    def filterLarger ( p : String, v : Any ) 
      = filter( p, v, Operator.Larger )
    def filterContains ( p : String, v : Any ) 
      = filter( p, v, Operator.Contains )
    def filterIn ( p : String, v : Any ) 
      = filter( p, v, Operator.In )
    def filterIncludes ( p : String, v : Any ) 
      = filter( p, v, Operator.Includes )
    def filterConstitutes ( p : String, v : Any ) 
      = filter( p, v, Operator.Constitutes )
    def filterLike ( p : String, v : Any ) 
      = filter( p, v, Operator.Like )
    def filterRegex ( p : String, v : Any ) 
      = filter( p, v, Operator.Regex )

    private[vorm] def query( kind : Kind = Kind.Select )
      = Query(kind, queryMapping, queryWhere, queryOrder, queryLimit, queryOffset)

    private def statementAndResultMappings ( q : Query )
      = {
        val sql = StandardSqlComposition.sql(AbstractSqlComposition.resultSetSelect(q))
        Statement( sql.template, sql.data map JdbcValue.apply ) ->
        q.mapping.resultSetMappings
      }

    def fetchAll()
      : Seq[T with Persisted]
      = {
        val (stmt, resultSetMappings)
          = statementAndResultMappings( query(Kind.Select) )

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
        ???
      }

  }
