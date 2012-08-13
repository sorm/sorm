package vorm.api

import org.joda.time.DateTime
import java.sql.DriverManager

import vorm._
import dropAll._
import persisted._
import query._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import create._
import drop._
import extensions._

trait Api {

  protected def connection
    : ConnectionAdapter with
      SaveAdapter with
      DropAllTablesAdapter

  protected def mappings
    : Map[Reflection, EntityMapping]

  private def mapping
    [ T : TypeTag ]
    = mappings.get(Reflection[T]) match {
        case Some(m) => m
        case None =>
          throw new RuntimeException( "Entity `" + Reflection[T].name + "` is not registered" )
      }

  def save
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = connection.saveEntityAndGetIt( value, mapping[T] )
        .asInstanceOf[T with Persisted]

  def query
    [ T <: AnyRef : TypeTag ]
    : Fetcher[T]
    = new Fetcher[T](connection, mapping[T])

  def fetchById
    [ T <: AnyRef : TypeTag ]
    ( id : Long )
    : Option[T with Persisted]
    = new Fetcher[T](connection, mapping[T]).filterEquals("id", id).fetchOne()

  /**
   * Current time at DB server
   */
  def fetchDate() : DateTime
    = connection
        .executeQuery( Statement("SELECT NOW()") )
        .parseAndClose().head.head
        .asInstanceOf[DateTime]

}
