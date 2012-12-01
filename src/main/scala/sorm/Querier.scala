package sorm

import sext._, embrace._
import reflect.runtime.universe._

import sorm._
import mappings._
import query._
import query.AbstractSqlComposition
import query.Query._
import persisted._
import core._

class Querier [ T <: AnyRef : TypeTag ] ( query : Query, connector : Connector ) {

  /**
   * Fetch matching entities from db.
   * @return A stream of entity instances with [[sorm.Persisted]] mixed in
   */
  def fetch () : Stream[T with Persisted]
    = fetchIds()
        .map("id" -> _).map(Map(_))
        .map{ pk =>
          connector.withConnection { cx =>
            query.mapping.fetchByPrimaryKey(pk, cx).asInstanceOf[T with Persisted]
          }
        }

  /**
   * Fetch ids of matching entities stored in db.
   * @return A stream of ids
   */
  def fetchIds () : Stream[Long]
    = connector.withConnection { cx =>
        query $ AbstractSqlComposition.primaryKeySelect $ (cx.query(_)(_.byNameRowsTraversable.toList)) $ (_.toStream) map (_("id") $ Util.toLong)
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
    = Query(query.mapping, where, order, amount, offset) $ (new Querier[T](_, connector))

  /**
   * Add an ordering instruction
   */
  def order ( p : String, reverse : Boolean = false ) : Querier[T]
    = query.order.toVector :+ Order(Path.mapping(query.mapping, p), reverse) $ (x => copy(order = x))

  /**
   * Limit the amount of entities to be fetched
   */
  def limit ( amount : Int ) = amount $ (Some(_)) $ (x => copy(amount = x))

  /**
   * Set the amount of first results to skip
   */
  def offset ( offset : Int ) = offset $ (x => copy(offset = x))

  /**
   * Return a copy of this `Querier` object with a filter generated from DSL.
   *
   * Usage of this method should be accompanied with {{{import sorm.Dsl._}}}
   * 
   */
  def where ( f : Querier.Filter )
    : Querier[T]
    = {
      def queryWhere (f : Querier.Filter) : Where
        = {
          def pvo
            = f match {
                case Querier.Equal(p, v)          => (p, v, Equal)
                case Querier.NotEqual(p, v)       => (p, v, NotEqual)
                case Querier.Larger(p, v)         => (p, v, Larger)
                case Querier.LargerOrEqual(p, v)  => (p, v, LargerOrEqual)
                case Querier.Smaller(p, v)        => (p, v, Smaller)
                case Querier.SmallerOrEqual(p, v) => (p, v, SmallerOrEqual)
                case Querier.Like(p, v)           => (p, v, Like) 
                case Querier.NotLike(p, v)        => (p, v, NotLike) 
                case Querier.Regex(p, v)          => (p, v, Regex) 
                case Querier.NotRegex(p, v)       => (p, v, NotRegex) 
                case Querier.In(p, v)             => (p, v, In) 
                case Querier.NotIn(p, v)          => (p, v, NotIn) 
                case Querier.Contains(p, v)       => (p, v, Contains) 
                case Querier.NotContains(p, v)    => (p, v, NotContains) 
                case Querier.Constitutes(p, v)    => (p, v, Constitutes) 
                case Querier.NotConstitutes(p, v) => (p, v, NotConstitutes) 
                case Querier.Includes(p, v)       => (p, v, Includes) 
                case Querier.NotIncludes(p, v)    => (p, v, NotIncludes) 
                case _ => throw new SormException("No operator for filter `" + f + "`")

              }
          f match {
            case Querier.Or(l, r) => 
              Or(queryWhere(l), queryWhere(r))
            case Querier.And(l, r) => 
              And(queryWhere(l), queryWhere(r))
            case _ =>
              pvo match { case (p, v, o) => Path.where(query.mapping, p, v, o) }
          }
        }
      where(queryWhere(f))
    }
  private def where ( w : Where )
    : Querier[T]
    = w +: query.where.toList reduceOption And $ (x => copy(where = x))

  @inline private def where ( p : String, v : Any, o : Operator )
    : Querier[T]
    = Path.where(query.mapping, p, v, o) $ where

  /**
   * Return a copy of this `Querier` object with an equality filter applied.
   * 
   * @param p A string indicating a path to the property on which to filter
   * @param v A value to compare with
   * @return A new instance of `Querier` with this filter applied
   */
  def whereEqual ( p : String, v : Any )
    = where( p, v, Equal )

  def whereNotEqual ( p : String, v : Any )
    = where( p, v, NotEqual )

  def whereLarger ( p : String, v : Any )
    = where( p, v, Larger )

  def whereLargerOrEqual ( p : String, v : Any )
    = where( p, v, LargerOrEqual )

  def whereSmaller ( p : String, v : Any ) 
    = where( p, v, Smaller )

  def whereSmallerOrEqual ( p : String, v : Any )
    = where( p, v, SmallerOrEqual )

  def whereLike( p : String, v : Any ) 
    = where( p, v, Like )

  def whereNotLike( p : String, v : Any ) 
    = where( p, v, NotLike )

  def whereRegex( p : String, v : Any ) 
    = where( p, v, Regex )

  def whereNotRegex( p : String, v : Any ) 
    = where( p, v, NotRegex )

  def whereIn ( p : String, v : Any ) 
    = where( p, v, In )

  def whereNotIn ( p : String, v : Any ) 
    = where( p, v, NotIn )

  def whereContains ( p : String, v : Any ) 
    = where( p, v, Contains )

  def whereNotContains ( p : String, v : Any ) 
    = where( p, v, NotContains )

  def whereConstitutes ( p : String, v : Any ) 
    = where( p, v, Constitutes )

  def whereNotConstitutes ( p : String, v : Any ) 
    = where( p, v, NotConstitutes )

  def whereIncludes ( p : String, v : Any ) 
    = where( p, v, Includes )

  def whereNotIncludes ( p : String, v : Any ) 
    = where( p, v, NotIncludes )

}
object Querier {
  def apply [ T <: AnyRef : TypeTag ] ( mapping : EntityMapping, connector : Connector )
    = new Querier[T]( Query(mapping), connector )

  sealed trait Filter
  case class Or ( l : Filter, r : Filter ) extends Filter
  case class And ( l : Filter, r : Filter ) extends Filter

  case class Equal ( p : String, v : Any ) extends Filter
  case class NotEqual ( p : String, v : Any ) extends Filter
  case class Larger ( p : String, v : Any ) extends Filter
  case class LargerOrEqual ( p : String, v : Any ) extends Filter
  case class Smaller ( p : String, v : Any ) extends Filter
  case class SmallerOrEqual ( p : String, v : Any ) extends Filter
  case class Like ( p : String, v : Any ) extends Filter
  case class NotLike ( p : String, v : Any ) extends Filter
  case class Regex ( p : String, v : Any ) extends Filter
  case class NotRegex ( p : String, v : Any ) extends Filter
  case class In ( p : String, v : Any ) extends Filter
  case class NotIn ( p : String, v : Any ) extends Filter
  case class Contains ( p : String, v : Any ) extends Filter
  case class NotContains ( p : String, v : Any ) extends Filter
  /**
   * Makes part of a collection
   */
  case class Constitutes ( p : String, v : Any ) extends Filter
  case class NotConstitutes ( p : String, v : Any ) extends Filter
  /**
   * Includes a collection
   */
  case class Includes ( p : String, v : Any ) extends Filter
  case class NotIncludes ( p : String, v : Any ) extends Filter

}
