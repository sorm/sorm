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

class Instance
  ( url : String,
    user : String,
    password : String,
    entities : Traversable[Entity[_]],
    mode : Mode = Mode.None )
  extends LogulaLogging
  {
    protected val connection
      = new ConnectionAdapter(DriverManager.getConnection(url, user, password))
          with SaveAdapter
          with DropAllTablesAdapter

    protected val mappings
      = {
        val settings
          = entities.view.map{ e => e.reflection -> e.settings }.toMap

        settings.keys
          .zipBy{ new EntityMapping(None, _, settings) }
          .toMap
      }

    mode match {
      case Mode.DropAllCreate =>
        connection.dropAllTables()
        for( s <- Create.statements(mappings.values) ){
          connection.executeUpdate(s)
        }
      case Mode.DropCreate =>
        for( s <- Drop.statements(mappings.values) ){
          try {
            connection.executeUpdate(s)
          } catch {
            case e : Throwable =>
              log.warn("Couldn't drop table. " + e.getMessage)
          }
        }
        for( s <- Create.statements(mappings.values) ){
          connection.executeUpdate(s)
        }
      case Mode.Create =>
        for( s <- Create.statements(mappings.values) ){
          connection.executeUpdate(s)
        }
      case Mode.None =>
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
      = connection.saveEntityAndGetIt( value, mapping[T] )
          .asInstanceOf[T with Persisted]

  }