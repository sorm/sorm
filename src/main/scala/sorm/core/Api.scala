package sorm.core

import reflect.basis._
import sorm._
import persisted._
import query.AbstractSqlComposition
import reflection._
import mappings._
import jdbc._
import query.Query._
import sext._

import com.weiglewilczek.slf4s.Logging

trait Api extends Logging with CurrentDateTime {

  protected def driver : Driver

  protected def mappings : Map[Reflection, EntityMapping]

  private def mapping
    [ T : TypeTag ]
    = {
      def mapping( r : Reflection ) 
        = mappings.get(r)
            .getOrElse {
              throw new SormException(
                "Entity `" + r.name + "` is not registered"
              )
            }
      mapping(Reflection[T].mixinBasis)
    }

  def access [ T <: AnyRef : TypeTag ] = Access[T](mapping, driver)

  def save
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = transaction {
        mapping[T].save(value).asInstanceOf[T with Persisted]
      }

  private def fetch [ T <: AnyRef ] ( q : Query ) : Seq[T with Persisted]
    = {
      val ids = q $ AbstractSqlComposition.primaryKeySelect $ (driver.query(_)(_.byNameRowsTraversable.toList)) $ (_.toStream)
      ids.map(q.mapping.fetchByPrimaryKey(_).asInstanceOf[T with Persisted])
    }

  def all
    [ T <: AnyRef : TypeTag ]
    = new FetchableQuery(
        Query(mapping[T]),
        fetch[T]
      )

  def one
    [ T <: AnyRef : TypeTag ]
    = new FetchableQuery(
        Query(mapping[T], limit = Some(1)),
        ( q : Query ) => fetch[T](q).headOption
      )

  def count
    [ T <: AnyRef : TypeTag ]
    = {
      logger.warn("Effective `count` query is not yet implemented. Using ineffective version")
      new FetchableQuery(
        Query(mapping[T]),
        fetch[T] _ andThen (_.size)
      )
    }

  def exists
    [ T <: AnyRef : TypeTag ]
    = {
      logger.warn("Effective `exists` query is not yet implemented. Using ineffective version")
      new FetchableQuery(
        Query(mapping[T], limit = Some(1)),
        fetch[T] _ andThen (_.nonEmpty)
      )
    }

  def fetchById
    [ T <: AnyRef : TypeTag ]
    ( id : Long )
    : Option[T with Persisted]
    = one[T].filterEqual("id", id).fetch()

  /**
   * Returns a query which when executed either updates the matched result with
   * the value or just saves the value
   */
  def overwrite
    [ T <: AnyRef : TypeTag ]
    ( v : T )
    = {
      val t = typeTag[T]
      new FetchableQuery(
        Query(mapping[T], limit = Some(1)),
        q => transaction {
          fetch[T](q).headOption.map(_.id).map(Persisted(v, _)) match {
            case Some(p) => save(p)(t.asInstanceOf[TypeTag[T with Persisted]])
            case None => save(v)
          }
        }
      )
    }

  def transaction [ T ] ( t : => T ) : T = driver.transaction(t)
  def transaction [ T ] ( t : Api => T ) : T = driver.transaction(t(this))

}
