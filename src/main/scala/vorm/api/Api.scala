package vorm.api

import org.joda.time.DateTime
import java.sql.DriverManager

import vorm._
import dropAll._
import persisted._
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

  /**
   * Current time at DB server
   */
  def date : DateTime
    = ???

  def save
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = connection.saveEntityAndGetIt( value, mapping[T] )
        .asInstanceOf[T with Persisted]

  def all
    [ T <: AnyRef : TypeTag ]
    : PowerQuery[T]
    = new PowerQuery[T](connection, mapping[T])

}
