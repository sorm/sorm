package sorm.core

import sorm._
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
import sext.Sext._

import query.Query._

import com.weiglewilczek.slf4s.Logging

class FetchableQuery
  [ T ]
  ( query : Query,
    fetchFunction : Query => T )
  {
    def fetch() = fetchFunction(query)

    private def copy
      ( kind    : Kind          = query.kind,
        mapping : TableMapping  = query.mapping,
        where   : Option[Where] = query.where,
        order   : Seq[Order]    = query.order,
        limit   : Option[Int]   = query.limit,
        offset  : Int           = query.offset )
      = new FetchableQuery[T](
          Query(kind, mapping, where, order, limit, offset),
          fetchFunction
        )

    private def order ( p : String, desc : Boolean = false )
      = copy(
          order
            = query.order.toVector :+
              Order(Path.mapping(query.mapping, p), desc)
        ) 

    def orderAsc ( p : String )
      = order(p, false)

    def orderDesc ( p : String )
      = order(p, true)

    def limit ( x : Int )
      = copy( limit = Some(x) )

    def offset ( x : Int )
      = copy( offset = x )

    def filter ( f : ApiFilter.Filter )
      : FetchableQuery[T]
      = {
        def queryWhere (f : ApiFilter.Filter) : Where
          = f match {
              case ApiFilter.Equal(p, v) => 
                Path.where( query.mapping, p, v, Operator.Equal)
              case ApiFilter.NotEqual(p, v) => 
                Path.where( query.mapping, p, v, Operator.NotEqual)
              case ApiFilter.Larger(p, v) =>
                Path.where( query.mapping, p, v, Operator.Larger)
              case ApiFilter.LargerOrEqual(p, v) =>
                Path.where( query.mapping, p, v, Operator.LargerOrEqual)
              case ApiFilter.Smaller(p, v) =>
                Path.where( query.mapping, p, v, Operator.Smaller)
              case ApiFilter.SmallerOrEqual(p, v) =>
                Path.where( query.mapping, p, v, Operator.SmallerOrEqual)
              case ApiFilter.Or(l, r) => 
                Or(queryWhere(l), queryWhere(r))
              case ApiFilter.And(l, r) => 
                And(queryWhere(l), queryWhere(r))
            }
        filter(queryWhere(f))
      }
    private def filter ( w : Where )
      : FetchableQuery[T]
      = copy( 
          where = (query.where ++: w +: Nil) reduceOption And
        )
    @inline private def filter ( p : String, v : Any, o : Operator )
      : FetchableQuery[T]
      = filter( Path.where( query.mapping, p, v, o ) )

    def filterEqual ( p : String, v : Any )
      = filter( p, v, Operator.Equal )

    def filterNotEqual ( p : String, v : Any )
      = filter( p, v, Operator.NotEqual )

    def filterLarger ( p : String, v : Any )
      = filter( p, v, Operator.Larger )

    def filterLargerOrEqual ( p : String, v : Any )
      = filter( p, v, Operator.LargerOrEqual )

    def filterSmaller ( p : String, v : Any ) 
      = filter( p, v, Operator.Smaller )

    def filterSmallerOrEqual ( p : String, v : Any )
      = filter( p, v, Operator.SmallerOrEqual )

    def filterLike( p : String, v : Any ) 
      = filter( p, v, Operator.Like )

    def filterNotLike( p : String, v : Any ) 
      = filter( p, v, Operator.NotLike )

    def filterRegex( p : String, v : Any ) 
      = filter( p, v, Operator.Regex )

    def filterNotRegex( p : String, v : Any ) 
      = filter( p, v, Operator.NotRegex )

    def filterIn ( p : String, v : Any ) 
      = filter( p, v, Operator.In )

    def filterNotIn ( p : String, v : Any ) 
      = filter( p, v, Operator.NotIn )

    def filterContains ( p : String, v : Any ) 
      = filter( p, v, Operator.Contains )

    def filterNotContains ( p : String, v : Any ) 
      = filter( p, v, Operator.NotContains )

    def filterConstitutes ( p : String, v : Any ) 
      = filter( p, v, Operator.Constitutes )

    def filterNotConstitutes ( p : String, v : Any ) 
      = filter( p, v, Operator.NotConstitutes )

    def filterIncludes ( p : String, v : Any ) 
      = filter( p, v, Operator.Includes )

    def filterNotIncludes ( p : String, v : Any ) 
      = filter( p, v, Operator.NotIncludes )

  }
