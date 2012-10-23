package sorm.api

import sext._, embrace._
import sorm._
import connection.Connection
import mappings._
import query.AbstractSqlComposition
import query.Query._
import persisted._
import reflect.runtime.universe.TypeTag

object Access {
  def apply [ T <: AnyRef : TypeTag ] ( mapping : EntityMapping, connection : Connection )
    = new Access[T]( Query(mapping), connection )
}
//  alternative titles: QueryApi
class Access [ T <: AnyRef : TypeTag ] ( query : Query, connection : Connection ) {

  /**
   * Fetch matching entities from db.
   * @return A stream of entity instances with [[sorm.persisted.Persisted]] mixed in
   */
  def fetch () : Stream[T with Persisted]
    = fetchIds().map("id" -> _).map(Map(_)).map(query.mapping.fetchByPrimaryKey(_, connection).asInstanceOf[T with Persisted])

  /**
   * Fetch ids of matching entities stored in db.
   * @return A stream of ids
   */
  def fetchIds () : Stream[Long]
    = query $ AbstractSqlComposition.primaryKeySelect $ (connection.query(_)(_.byNameRowsTraversable.toList)) $ (_.toStream) map (_("id").asInstanceOf[Long])

  /**
   * Fetch only one entity ensuring that `amount(1)` is applied to the query.
   * @return An option of entity instance with [[sorm.persisted.Persisted]] mixed in
   */
  def fetchOne () : Option[T with Persisted]
    = amount(1).fetch().headOption
    
  /**
   * Fetch only one id ensuring that `amount(1)` is applied to the query.
   */
  def fetchOneId () : Option[Long]
    = amount(1).fetchIds().headOption

  /**
   * Fetch a number of matching entities stored in db.
   */
  //  todo: implement effective version
  def count () : Int 
    = fetchIds().size

  /**
   * Fetch a boolean value indicating whether any matching entities are stored in db.
   */
  //  todo: implement effective version
  def exists () : Boolean
    = fetchOneId().nonEmpty

  /**
   * Replace all matching entities stored in db with the value provided
   * @param value A value to replace the existing entities with
   * @return A list of saved entities with [[sorm.persisted.Persisted]] mixed in
   */
  def replace ( value : T ) : List[T with Persisted]
    = connection.transaction {
        fetchIds().map(Persisted(value, _)).map(query.mapping.save(_, connection).asInstanceOf[T with Persisted]).toList
      }



  private def copy
    ( where   : Option[Where] = query.where,
      order   : Seq[Order]    = query.order,
      amount  : Option[Int]   = query.limit,
      offset  : Int           = query.offset )
    = Query(query.mapping, where, order, amount, offset) $ (new Access[T](_, connection))

  def order ( p : String, reverse : Boolean = false ) : Access[T]
    = query.order.toVector :+ Order(Path.mapping(query.mapping, p), reverse) $ (x => copy(order = x))

  /**
   * Limit the amount of entities to be fetched
   */
  def amount ( amount : Int ) = amount $ (Some(_)) $ (x => copy(amount = x))

  /**
   * Set the amount of first results to skip
   */
  def offset ( offset : Int ) = offset $ (x => copy(offset = x))

  /**
   * Return a copy of this `Access` object with a filter generated from DSL.
   *
   * Usage of this method should be accompanied with {{import sorm.Sorm.FilterDsl._}}
   * 
   */
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

  /**
   * Return a copy of this `Access` object with an equality filter applied. 
   * 
   * @param p A string indicating a path to the property on which to filter
   * @param v A value to compare with
   * @return A new instance of `Access` with this filter applied 
   */
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
