package vorm.api

import org.joda.time.DateTime
import java.sql.DriverManager

import vorm._
import persisted._
import reflection._
import save._
import structure._
import mapping._
import jdbc._
import extensions._

class Instance
  ( url : String,
    user : String,
    password : String,
    entities : Traversable[Entity[_]] )
  {
    private val api
      = new ConnectionAdapter(DriverManager.getConnection(url, user, password))
          with SaveAdapter

    private val mappings
      = {
        val settings
          = {
            def settings
              ( e : Entity[_] )
              = EntitySettings(e.primaryKey, e.uniqueKeys, e.indexes, e.autoIncrement)

            entities.view.map{ e => e.reflection -> settings(e) }.toMap
          }

        settings.keys
          .zipBy{ new EntityMapping(None, _, settings) }
          .toMap
      }

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
      = api.saveEntityAndGetIt( value, mapping[T] )
          .asInstanceOf[T with Persisted]

  }