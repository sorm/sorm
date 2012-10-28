package sorm

import sorm._
import driver.Driver
import core._
import persisted._
import reflection._
import mappings._
import jdbc._

import sext._, embrace._
import com.weiglewilczek.slf4s.Logging

/**
 * The instance of SORM
 * @param entities A list of entity settings describing the entities to be
 *                 registered with this instance
 * @param url A url of database to connect to. For instance, to connect to a
 *            database `test` on MySQL server running on your computer it will
 *            be: `jdbc:mysql://localhost/test`
 * @param user A username used for connection
 * @param password A password used for connection
 * @param initMode An initialization mode for this instance
 */
class Instance
  ( entities : Traversable[Entity],
    url : String,
    user : String = "",
    password : String = "",
    initMode : InitMode = InitMode.DoNothing )
  extends Logging
  {
    class ValidationException ( m : String ) extends SormException(m)

    private val driver = Driver(url, user, password)

    /**
     * Open a new connection to the db and return the API to work with it
     */
    def connection ()
      = new Connection {
          protected val connection = driver.connection()
          protected def mappings = Instance.this.mappings
        }

    /**
     * Perform some actions on a connection which will be closed after
     */
    def withTmpConnection [ T ] ( f : Connection => T )
      = driver.withTmpConnection(c => f(new Connection { protected def connection = c; protected def mappings = Instance.this.mappings }))

    //  Validate entities (must be prior to mappings creation due to possible mappingkind detection errors):
    entities flatMap Initialization.errors map (new ValidationException(_)) foreach (throw _)

    private val mappings
      = {
        val settings
          = entities.view
              .map{ e =>
                e.reflection -> EntitySettings(e.indexes, e.uniqueKeys)
              }
              .toMap

        settings.keys
          .zipBy{ new EntityMapping(_, None, settings) }
          .toMap
      }

    // Validate input:
    mappings.values.toStream $ Initialization.errors map (new ValidationException(_)) foreach (throw _)

    // Initialize a db schema:
    Initialization.initializeSchema(mappings.values, driver, initMode)

    // Precache persisted classes (required for multithreading)
    entities.toStream
      .map(_.reflection)
      .foreach(PersistedClass.apply)

  }

