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

trait Api extends Logging {

  protected def connection
    : ConnectionAdapter with
      SaveAdapter

  protected def mappings
    : Map[Reflection, EntityMapping]

  private def mapping
    [ T : TypeTag ]
    = mappings.get(Reflection[T])
        .getOrElse{
          throw new SormException(
            "Entity `" + Reflection[T].name + "` is not registered"
          )
        }

  def save
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = connection.saveEntityAndGetIt( value, mapping[T] )
        .asInstanceOf[T with Persisted]


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

      connection.executeQuery(stmt)
        .fetchInstancesAndClose(
          q.mapping,
          resultSetMappings.view.zipWithIndex.toMap
        )
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
   * Current DateTime at DB server
   */
  def fetchDate() : DateTime
    = connection
        .executeQuery( Statement("SELECT NOW()") )
        .parseAndClose().head.head
        .asInstanceOf[DateTime]

  /**
   * Returns a query which when executed either updates the matched result with 
   * the value or just saves the value
   */
  def update
    [ T <: AnyRef : TypeTag ]
    ( v : T ) 
    = new FetchableQuery(
        Query(Kind.Select, mapping[T], limit = Some(1)),
        fetch[T] _ andThen (_.headOption) andThen 
        (_ map (_.id) map (Persisted(v, _)) map (save(_)) getOrElse (save(v)))
      )
  
  /**
   * If an entity with all fields matching is already saved, return it,
   * otherwise save this one and return its persisted copy.
   */
  def fetchOrSave
    [ T <: AnyRef : TypeTag ]
    ( v : T ) 
    : T with Persisted
    = v match {
        case v : Persisted =>
          v.asInstanceOf[T with Persisted]
        case v =>
          v.reflected.propertyValues
            .foldLeft( one[T] ){ case (q, (n, v)) => q.filterEqual(n, v) }
            .fetch()
            .getOrElse( save(v).asInstanceOf[T with Persisted] )
      }

}
