package sorm

import sorm._
import core._
import persisted._
import reflection._
import mappings._
import jdbc._

import sext._, embrace._
import reflect.runtime.universe._
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
 * @param poolSize A size of connection pool. Determines how many connections
 *                 to the db will be kept at max. Useful for multithreaded
 *                 databases.
 * @param initMode An initialization mode for this instance
 */
class Instance
  ( entities : Traversable[Entity],
    url : String,
    user : String = "",
    password : String = "",
    poolSize : Int = 1,
    initMode : InitMode = InitMode.Create )
  extends Api with Logging
  {
    class ValidationException ( m : String ) extends SormException(m)

    protected val connector = new Connector(url, user, password, poolSize)

    protected def mapping
      [ T : TypeTag ]
      = {
        def mapping( r : Reflection )
          = mappings.get(r)
              .getOrElse {
                throw new SormException(
                  "Entity `" + r.name + "` is not registered"
                )
              }
        mapping(Reflection[T].mixinBasis)
      }

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
    Initialization.initializeSchema(mappings.values, connector, initMode)

    // Precache persisted classes (required for multithreading)
    entities.foreach(_.reflection $ PersistedClass.apply)

  }

