package sorm

import sorm._
import core._
import dropAll._
import persisted._
import reflection._
import save._
import structure._
import structure.mapping._
import jdbc._
import create._
import drop._
import sext.Sext._

import com.weiglewilczek.slf4s.Logging

import reflect.basis._

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
    extends Api
    with Logging
    {
      protected[sorm] val connection
        = new ConnectionAdapter(JdbcConnection(url, user, password))
            with SaveAdapter
            with DropAllTablesAdapter

      protected[sorm] val mappings
        = {
          val settings
            = entities.view
                .map{ e =>
                  e.reflection -> EntitySettings(e.indexes, e.uniqueKeys)
                }
                .toMap

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
              if( !mappings.contains(e1.reflection) )
                throw new ValidationException(
                  "Entity `" + e1.reflection.name + "` is not registered, " +
                  "but referred to in `" + e.reflection.name + "`"
                )

            }
          }
        }
        // No reflection should be registered twice
        {
          val reflections = entities.toStream.map{_.reflection}
          val diff = reflections.distinct diff reflections
          if( diff != Stream() )
            throw new ValidationException(
              "Reflections registered twice: " + diff.mkString(", ")
            )
        }
      }

      // Initialize a db schema:
      {
        initMode match {
          case InitMode.DropAllCreate =>
            try {
              connection.dropAllTables()
            } catch {
              case e : Throwable =>
                logger.warn("Couldn't drop all tables.")
            }
            for( s <- Create.statements(mappings.values) ){
              connection.executeUpdate(s)
            }
          case InitMode.DropCreate =>
            for( s <- Drop.statements(mappings.values) ){
              try {
                connection.executeUpdate(s)
              } catch {
                case e : Throwable =>
                  logger.warn("Couldn't drop table. " + e.getMessage)
              }
            }
            for( s <- Create.statements(mappings.values) ){
              connection.executeUpdate(s)
            }
          case InitMode.Create =>
            for( s <- Drop.statements(mappings.values) ){
              try {
                for( s <- Create.statements(mappings.values) ){
                  connection.executeUpdate(s)
                }
              } catch {
                case e : Throwable =>
                  logger.warn("Couldn't create table. " + e.getMessage)
              }
            }
          case InitMode.DoNothing =>
        }
      }

      // Precache persisted classes (required for multithreading)
      {
        entities.toStream
          .map(_.reflection)
          .foreach(PersistedClass.apply)
      }

    }

  sealed case class Entity
    ( reflection    : Reflection,
      indexes       : Set[Seq[String]],
      uniqueKeys    : Set[Seq[String]] )
    {
      //  Validate input:
      // TODO : should be moved to Instance which in its turn should factor validation out
      {
        lazy val descendats
          = reflection.properties.values
              .unfold( a => a.notEmpty.map(a => a -> a.flatMap(_.generics)) )
              .flatten

        descendats
          .filter(r => r =:= Reflection[Any] || r =:= Reflection[AnyRef] || r =:= Reflection[AnyVal])
          .foreach(r => throw new ValidationException(s"Specifying general types `Any`, `AnyRef` or `AnyVal` is not allowed."))

        descendats
          .filter(_ <:< Reflection[TraversableOnce[_]])
          .filterNot(r =>
            //  using java class to erase generics
            r.javaClass == classOf[Seq[_]] ||
            r.javaClass == classOf[Set[_]] ||
            r.javaClass == classOf[Map[_, _]] ||
            r.javaClass == classOf[Range]
          )
          .foreach(r =>
            throw new ValidationException(
              s"Only general immutable `Seq`, `Set`, `Map` and `Range` are supported traversable types. `$r` detected instead"
            )
          )


        {
          descendats
            .filter{ _ <:< Reflection[Option[_]] }
            .foreach{ r =>
              if( r.generics(0).<:<(Reflection[Option[_]]) )
                throw new ValidationException(
                  "Type signatures with `Option` being directly nested " +
                  "in another `Option`, i.e. `Option[Option[_]]` are " +
                  "not supported" 
                )
              if( r.generics(0).<:<(Reflection[Traversable[_]]) )
                throw new ValidationException(
                  "Type signatures with collection being directly nested" +
                  " in `Option`, e.g. `Option[Seq[_]]` are not supported" 
                )
            }
        }

        ( indexes.view ++ uniqueKeys.view )
          .flatten
          .foreach{ p =>
            if( !reflection.properties.contains(p) )
              throw new ValidationException(s"Inexistent property: `$p`")
          }

        ( indexes.view ++ uniqueKeys.view )
          .foreach{ ps =>
            if( ps.distinct.size != ps.size )
              throw new ValidationException(
                "Not a distinct properties list: `" + ps.mkString(", ") + "`"
              )
          }
      }
    }
  object Entity {
    /**
     * Entity settings. Used for registring entities with the SORM instance.
     * @param indexes
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
      ( indexes    : Set[Seq[String]] = Set(),
        unique : Set[Seq[String]] = Set() )
      : Entity
      = Entity(Reflection[T], indexes, unique)
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

    import core.ApiFilter._

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
