package sorm.core

import org.joda.time.DateTime

import reflect.basis._
import sorm._
import persisted._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import query.Query._
import sext.Sext._

import abstractSql.StandardSqlComposition
import query.AbstractSqlComposition
import sql.StandardRendering._
import resultSet._
import com.weiglewilczek.slf4s.Logging

trait Api extends Logging with CurrentDateTime {

  protected def connection
    : ConnectionAdapter with
      SaveAdapter

  protected def mappings
    : Map[Reflection, EntityMapping]

  private def mapping
    [ T : TypeTag ]
    = {
      def mapping( r : Reflection ) =
        mappings.get(r)
          .getOrElse {
            throw new SormException(
              "Entity `" + r.name + "` is not registered"
            )
          }
      mapping(Reflection[T].mixinBasis)
    }

  def save
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = transaction {
        connection.saveEntityAndGetIt( value, mapping[T] )
          .asInstanceOf[T with Persisted]
      }


  private def statementAndResultMappings ( q : Query )
    = {
      val sql 
        = StandardSqlComposition.sql(AbstractSqlComposition.resultSetSelect(q))
      Statement( sql.template, sql.data map JdbcValue.apply ) ->
      q.mapping.resultSetMappings
    }
  private def fetch [ T <: AnyRef ] ( q : Query )
    = {
      val (stmt, resultSetMappings) = statementAndResultMappings( q )

      connection.executeQuery(stmt)(_.parseInstances(q.mapping, resultSetMappings.view.zipWithIndex.toMap))
        .asInstanceOf[Seq[T with Persisted]]
    }

  def all
    [ T <: AnyRef : TypeTag ]
    = new FetchableQuery(
        Query(Kind.Select, mapping[T]),
        fetch[T]
      )

  def one
    [ T <: AnyRef : TypeTag ]
    = new FetchableQuery(
        Query(Kind.Select, mapping[T], limit = Some(1)),
        ( q : Query ) => fetch[T](q).headOption
      )

  def count
    [ T <: AnyRef : TypeTag ]
    = {
      logger.warn("Effective `count` query is not yet implemented. Using ineffective version")
      new FetchableQuery(
        Query(Kind.Select, mapping[T]),
        fetch[T] _ andThen (_.size)
      )
    }

  def exists
    [ T <: AnyRef : TypeTag ]
    = {
      logger.warn("Effective `exists` query is not yet implemented. Using ineffective version")
      new FetchableQuery(
        Query(Kind.Select, mapping[T], limit = Some(1)),
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
        Query(Kind.Select, mapping[T], limit = Some(1)),
        q => transaction {
          fetch[T](q).headOption.map(_.id).map(Persisted(v, _)) match {
            case Some(p) => save(p)(t.asInstanceOf[TypeTag[T with Persisted]])
            case None => save(v)
          }
        }
      )
    }

  def transaction [ T ] ( t : => T ) : T = connection.transaction(t)
  def transaction [ T ] ( t : Api => T ) : T = connection.transaction(t(this))

}
