package sorm

import sext._, embrace._
import sorm._
import sorm.{Filter => ApiFilter}
import driver.DriverConnection
import mappings._
import query.AbstractSqlComposition
import query.Query._, Operator._
import persisted._
import reflect.runtime.universe.TypeTag
import core._

object Access {
  def apply [ T <: AnyRef : TypeTag ] ( mapping : EntityMapping, connector : Connector )
    = new Access[T]( Query(mapping), connector )
}
//  @TODO: rename to Query
class Access [ T <: AnyRef : TypeTag ] ( query : Query, connector : Connector ) {

  /**
   * Fetch matching entities from db.
   * @return A stream of entity instances with [[sorm.Persisted]] mixed in
   */
  def fetch () : Stream[T with Persisted]
    = connector.withConnection { cx =>
        fetchIds().map("id" -> _).map(Map(_)).map(query.mapping.fetchByPrimaryKey(_, cx).asInstanceOf[T with Persisted])
      }

  /**
   * Fetch ids of matching entities stored in db.
   * @return A stream of ids
   */
  def fetchIds () : Stream[Long]
    = connector.withConnection { cx =>
        query $ AbstractSqlComposition.primaryKeySelect $ (cx.query(_)(_.byNameRowsTraversable.toList)) $ (_.toStream) map (_("id").asInstanceOf[Long])
      }

  /**
   * Fetch only one entity ensuring that `limit(1)` is applied to the query.
   * @return An option of entity instance with [[sorm.Persisted]] mixed in
   */
  def fetchOne () : Option[T with Persisted]
    = limit(1).fetch().headOption
    
  /**
   * Fetch only one id ensuring that `limit(1)` is applied to the query.
   */
  def fetchOneId () : Option[Long]
    = limit(1).fetchIds().headOption

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
   * @return A list of saved entities with [[sorm.Persisted]] mixed in
   */
  def replace ( value : T ) : List[T with Persisted]
    = connector.withConnection { cx =>
        cx.transaction {
          fetchIds().map(Persisted(value, _)).map(query.mapping.save(_, cx).asInstanceOf[T with Persisted]).toList
        }
      }

  private def copy
    ( where   : Option[Where] = query.where,
      order   : Seq[Order]    = query.order,
      amount  : Option[Int]   = query.limit,
      offset  : Int           = query.offset )
    = Query(query.mapping, where, order, amount, offset) $ (new Access[T](_, connector))

  /**
   * Add an ordering instruction
   */
  def order ( p : String, reverse : Boolean = false ) : Access[T]
    = query.order.toVector :+ Order(Path.mapping(query.mapping, p), reverse) $ (x => copy(order = x))

  /**
   * Limit the limit of entities to be fetched
   */
  def limit ( amount : Int ) = amount $ (Some(_)) $ (x => copy(amount = x))

  /**
   * Set the limit of first results to skip
   */
  def offset ( offset : Int ) = offset $ (x => copy(offset = x))

  /**
   * Return a copy of this `Access` object with a filter generated from DSL.
   *
   * Usage of this method should be accompanied with {{{import sorm.FilterDsl._}}}
   * 
   */
  def where ( f : ApiFilter.Filter )
    : Access[T]
    = {
      def queryWhere (f : ApiFilter.Filter) : Where
        = {
          def pvo
            = f match {
                case ApiFilter.Equal(p, v)          => (p, v, Equal)
                case ApiFilter.NotEqual(p, v)       => (p, v, NotEqual)
                case ApiFilter.Larger(p, v)         => (p, v, Larger)
                case ApiFilter.LargerOrEqual(p, v)  => (p, v, LargerOrEqual)
                case ApiFilter.Smaller(p, v)        => (p, v, Smaller)
                case ApiFilter.SmallerOrEqual(p, v) => (p, v, SmallerOrEqual)
                case ApiFilter.Like(p, v)           => (p, v, Like) 
                case ApiFilter.NotLike(p, v)        => (p, v, NotLike) 
                case ApiFilter.Regex(p, v)          => (p, v, Regex) 
                case ApiFilter.NotRegex(p, v)       => (p, v, NotRegex) 
                case ApiFilter.In(p, v)             => (p, v, In) 
                case ApiFilter.NotIn(p, v)          => (p, v, NotIn) 
                case ApiFilter.Contains(p, v)       => (p, v, Contains) 
                case ApiFilter.NotContains(p, v)    => (p, v, NotContains) 
                case ApiFilter.Constitutes(p, v)    => (p, v, Constitutes) 
                case ApiFilter.NotConstitutes(p, v) => (p, v, NotConstitutes) 
                case ApiFilter.Includes(p, v)       => (p, v, Includes) 
                case ApiFilter.NotIncludes(p, v)    => (p, v, NotIncludes) 
                case _ => throw new SormException("No operator for filter `" + f + "`")

              }
          f match {
            case ApiFilter.Or(l, r) => 
              Or(queryWhere(l), queryWhere(r))
            case ApiFilter.And(l, r) => 
              And(queryWhere(l), queryWhere(r))
            case _ =>
              pvo match { case (p, v, o) => Path.where(query.mapping, p, v, o) }
          }
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
