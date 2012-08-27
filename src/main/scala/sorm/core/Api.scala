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
import extensions.Extensions._

trait Api {

  protected def connection
    : ConnectionAdapter with
      SaveAdapter

  protected def mappings
    : Map[Reflection, EntityMapping]

  private def mapping
    [ T : TypeTag ]
    = mappings.get(Reflection[T])
        .getOrElse{
          throw new Exception( "Entity `" + Reflection[T].name + 
                               "` is not registered" )
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
    = new Fetcher[T](connection, mapping[T]).filterEqual("id", id).fetchOne()

  /**
   * Current time at DB server
   */
  def fetchDate() : DateTime
    = connection
        .executeQuery( Statement("SELECT NOW()") )
        .parseAndClose().head.head
        .asInstanceOf[DateTime]

}
