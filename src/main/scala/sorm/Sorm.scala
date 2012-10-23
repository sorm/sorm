package sorm

import driver.Driver
import sorm._
import api._
import core._
import persisted._
import reflection._
import mappings._
import jdbc._
import tableSorters._
import connection._

import sext._, embrace._
import com.weiglewilczek.slf4s.Logging

import reflect.runtime.universe._

object Sorm {

  class ValidationException ( m : String ) extends SormException(m)

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

      private val driver = Driver(url, user, password)

      def connection ()
        = new ConnectionApi {
            protected val connection = driver.connection()
            protected def mappings = Instance.this.mappings
          }

      def withTmpConnection [ T ] ( f : ConnectionApi => T )
        = driver.withTmpConnection(c => f(new ConnectionApi { protected def connection = c; protected def mappings = Instance.this.mappings }))

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
      driver.withTmpConnection{ c =>
        Initialization.initializeSchema(mappings.values, c, initMode)
      }

      // Precache persisted classes (required for multithreading)
      entities.toStream
        .map(_.reflection)
        .foreach(PersistedClass.apply)

    }

  sealed case class Entity
    ( reflection    : Reflection,
      indexes       : Set[Seq[String]],
      uniqueKeys    : Set[Seq[String]] )
  object Entity {
    /**
     * Entity settings. Used for registring entities with the SORM instance.
     * @param indexed
     * Fields of case class on which the filtering operations will be performed
     * when querying the db. Specifying those may help the DB to perform a little
     * better.
     * @param unique
     * Fields of case class which are known to have a unique value amongst all
     * possible instances of this class. Specifying these may help the db perform
     * a little better and will protect you from storing entities having duplicate
     * values of this field.
     * @tparam T The case class
     */
    def apply
      [ T : TypeTag ]
      ( indexed : Set[Seq[String]] = Set(),
        unique : Set[Seq[String]] = Set() )
      : Entity
      = Entity(Reflection[T], indexed, unique)
  }

  /**
   * The mode for initialization performed when a connection to the db is
   * established on creation of SORM instance.
   */
  sealed trait InitMode
  object InitMode {
    /**
     * Wipe out all the contents of the db and generate the tables
     */
    case object DropAllCreate extends InitMode
    /**
     * Drop only the tables which have conflicting names with the ones to be
     * generated and actually generate them
     */
    case object DropCreate extends InitMode
    /**
     * Just generate the tables. Fail if name conflicts arise with the existing
     * ones
     */
    case object Create extends InitMode
    /**
     * Do nothing
     */
    case object DoNothing extends InitMode
  }

  object FilterDsl {

    import api.ApiFilter._

    implicit class WhereWhere(val a: Filter) extends AnyVal {
      def or(b: Filter) = Or(a, b)
      def and(b: Filter) = And(a, b)
    }

    implicit class StringWhere(val p: String) extends AnyVal {
      def equal(v: Any)          = Equal(p, v)
      def notEqual(v: Any)       = NotEqual(p, v)
      def smaller(v: Any)        = Smaller(p, v)
      def smallerOrEqual(v: Any) = SmallerOrEqual(p, v)
      def larger(v: Any)         = Larger(p, v)
      def largerOrEqual(v: Any)  = LargerOrEqual(p, v)
      //  todo: add the rest
    }

  }

}
