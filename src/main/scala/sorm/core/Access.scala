package sorm.core

import sext._
import sorm._
import mappings._
import query.AbstractSqlComposition
import query.Query._
import persisted._
import reflect.runtime.universe.TypeTag

object Access {
  def apply [ T <: AnyRef : TypeTag ] ( mapping : EntityMapping, driver : Driver )
    = new Access[T]( Query(mapping), driver )
}
class Access [ T <: AnyRef : TypeTag ] ( query : Query, driver : Driver ) {

  def fetch () : Stream[T with Persisted]
    = fetchIds().map("id" -> _).map(Map(_)).map(query.mapping.fetchByPrimaryKey(_).asInstanceOf[T with Persisted])

  def fetchIds () : Stream[Long]
    = query $ AbstractSqlComposition.primaryKeySelect $ (driver.query(_)(_.byNameRowsTraversable.toList)) $ (_.toStream) map (_("id").asInstanceOf[Long])

  def fetchOne () : Option[T with Persisted]
    = amount(1).fetch().headOption
    
  def fetchOneId () : Option[Long]
    = amount(1).fetchIds().headOption

  //  todo: implement effective version
  def count () : Int 
    = fetchIds().size

  //  todo: implement effective version
  def exists () : Boolean
    = fetchOneId().nonEmpty

  def replace ( value : T ) : List[T with Persisted]
    = driver.transaction {
        fetchIds().map(Persisted(value, _)).map(query.mapping.save(_).asInstanceOf[T with Persisted]).toList
      }



  private def copy
    ( where   : Option[Where] = query.where,
      order   : Seq[Order]    = query.order,
      amount  : Option[Int]   = query.limit,
      offset  : Int           = query.offset )
    = Query(query.mapping, where, order, amount, offset) $ (new Access[T](_, driver))

  def order ( p : String, reverse : Boolean = false ) : Access[T]
    = query.order.toVector :+ Order(Path.mapping(query.mapping, p), reverse) $ (x => copy(order = x))

  def amount ( amount : Int ) = amount $ (Some(_)) $ (x => copy(amount = x))

  def offset ( offset : Int ) = offset $ (x => copy(offset = x))

  def where ( f : ApiFilter.Filter )
    : Access[T]
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
      where(queryWhere(f))
    }
  private def where ( w : Where )
    : Access[T]
    = w +: query.where.toList reduceOption And $ (x => copy(where = x))

  @inline private def where ( p : String, v : Any, o : Operator )
    : Access[T]
    = Path.where(query.mapping, p, v, o) $ where

  def whereEqual ( p : String, v : Any )
    = where( p, v, Operator.Equal )

  def whereNotEqual ( p : String, v : Any )
    = where( p, v, Operator.NotEqual )

  def whereLarger ( p : String, v : Any )
    = where( p, v, Operator.Larger )

  def whereLargerOrEqual ( p : String, v : Any )
    = where( p, v, Operator.LargerOrEqual )

  def whereSmaller ( p : String, v : Any ) 
    = where( p, v, Operator.Smaller )

  def whereSmallerOrEqual ( p : String, v : Any )
    = where( p, v, Operator.SmallerOrEqual )

  def whereLike( p : String, v : Any ) 
    = where( p, v, Operator.Like )

  def whereNotLike( p : String, v : Any ) 
    = where( p, v, Operator.NotLike )

  def whereRegex( p : String, v : Any ) 
    = where( p, v, Operator.Regex )

  def whereNotRegex( p : String, v : Any ) 
    = where( p, v, Operator.NotRegex )

  def whereIn ( p : String, v : Any ) 
    = where( p, v, Operator.In )

  def whereNotIn ( p : String, v : Any ) 
    = where( p, v, Operator.NotIn )

  def whereContains ( p : String, v : Any ) 
    = where( p, v, Operator.Contains )

  def whereNotContains ( p : String, v : Any ) 
    = where( p, v, Operator.NotContains )

  def whereConstitutes ( p : String, v : Any ) 
    = where( p, v, Operator.Constitutes )

  def whereNotConstitutes ( p : String, v : Any ) 
    = where( p, v, Operator.NotConstitutes )

  def whereIncludes ( p : String, v : Any ) 
    = where( p, v, Operator.Includes )

  def whereNotIncludes ( p : String, v : Any ) 
    = where( p, v, Operator.NotIncludes )

}
