package sorm

import sorm._
import api._
import dropAll._
import persisted._
import reflection._
import save._
import structure._
import structure.mapping._
import jdbc._
import create._
import drop._
import extensions.Extensions._

import com.weiglewilczek.slf4s.Logging

import reflect.basis._

object Sorm {

  sealed trait Mode
  object Mode {
    case object DropAllCreate extends Mode
    case object DropCreate extends Mode
    case object Create extends Mode
    case object None extends Mode
  }

  sealed case class Entity
    [ T : TypeTag ]
    ( indexes       : Set[Seq[String]] = Set(),
      uniqueKeys    : Set[Seq[String]] = Set() )
    {

      lazy val reflection
        = Reflection[T]
      def settings
        = EntitySettings(indexes, uniqueKeys)


      //  Validate input:
      {
        {
          def allDescendantGenerics
            ( r : Reflection )
            : Stream[Reflection]
            = r +:
              r.generics.view
                .flatMap{allDescendantGenerics}
                .toStream

          reflection.properties.values
            .flatMap{ allDescendantGenerics }
            .filter{ _ inheritsFrom Reflection[Option[_]] }
            .foreach{ r =>
              require( !r.generics(0).inheritsFrom(Reflection[Option[_]]),
                       "Type signatures with `Option` being directly nested in another `Option`, i.e. `Option[Option[_]]` are not allowed" )
              require( !r.generics(0).inheritsFrom(Reflection[Traversable[_]]),
                       "Type signatures with collection being directly nested in `Option`, e.g. `Option[Seq[_]]` are not allowed" )
            }
        }

        ( indexes.view ++ uniqueKeys.view )
          .flatten
          .foreach{ p =>
            require( reflection.properties.contains(p),
                     "Inexistent property: `" + p + "`" )
          }

        ( indexes.view ++ uniqueKeys.view )
          .foreach{ ps =>
            require( ps.view.distinct.size == ps.size,
                     "Not a distinct properties list: `" + ps.mkString(", ") + "`" )
          }
      }
    }

  class Instance
    ( entities : Traversable[Entity[_]],
      url : String,
      user : String = "",
      password : String = "",
      mode : Mode = Mode.None )
    extends Api
    with Logging
    {
      protected[sorm] val connection
        = new ConnectionAdapter(Connection(url, user, password))
            with SaveAdapter
            with DropAllTablesAdapter

      protected[sorm] val mappings
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
                  logger.warn("Couldn't drop table. " + e.getMessage)
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


}
