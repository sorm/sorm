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
  ( entities : Traversable[Entity[_]],
    url : String,
    user : String = "",
    password : String = "",
    mode : Mode = Mode.None )
  extends Api
  with LogulaLogging
  {
    protected val connection
      = new ConnectionAdapter(Connection(url, user, password))
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

    // Validate input:
    {
      // All referred entities must be registered
      {
        def nestedEntities
          ( m : Mapping )
          : Stream[EntityMapping]
          = m match {
              case m : EntityMapping => 
                Stream(m)
              case m : HasChildren => 
                m.children.toStream.flatMap{ nestedEntities }              
              case _ =>
                Stream()
            }

        mappings.values.foreach{ e =>
          e.children.flatMap{ nestedEntities }.foreach{ e1 =>
            require( mappings.contains(e1.reflection),
                     "Entity `" + e1 + "` is not registered, but referred " +
                     "in `" + e + "`" )
            
          }
        }
      }
      // No reflection should be registered twice
      {
        val reflections = entities.toStream.map{_.reflection}
        val diff = reflections.distinct diff reflections
        require( diff == Stream(),
                 "Reflections registered twice: " + diff.mkString(", ") )
      }
    }

    // Initialize a db schema:
    {
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
    }

  }