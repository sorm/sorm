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

  }