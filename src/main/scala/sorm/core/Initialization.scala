package sorm.core

import sorm._
import reflection._
import mappings._
import jdbc._
import tableSorters._

import sext._, embrace._
import com.typesafe.scalalogging.{StrictLogging => Logging}

object Initialization extends Logging {

  def validateMapping ( mappings : Stream[EntityMapping] ) : Stream[String]
    = mappings
        .zipBy(_.deepContainedMappings.collect{case m : EntityMapping => m})
        .map{case (m, refs) => m -> refs.filterNot(r => mappings.exists(_.reflection =:= r.reflection))}
        .filter(_._2.nonEmpty)
        .map(_ $$ ("Entity `" + _.reflection.name + "` refers to unregistered entities: " + _.map("`" + _.reflection.name + "`").mkString(", "))) ++
      {
        val reflections = mappings.map(_.reflection)
        val diff = reflections.distinct diff reflections
        diff.nonEmpty.option("Reflections registered twice: " + diff.mkString(", "))
      }

  def validateEntities ( entities : Seq[Entity] ) : Stream[String]
    = {
      lazy val entityTypes = entities.map(_.reflection)
      def validateEntity ( e : Entity ) : Stream[String]
        = {
          lazy val descendats
            = e.reflection.properties.values
                .unfold( a => a.notEmpty.map(a => a -> a.flatMap(_.generics)) )
                .flatten

          def containsId
            = e.reflection.properties.keys.exists(_ == "id").option("Property name `id` is not allowed")

          def generalTypes
            = descendats
                .filter(r => r =:= Reflection[Any] || r =:= Reflection[AnyRef] || r =:= Reflection[AnyVal])
                .map("Specifying general types `Any`, `AnyRef` or `AnyVal` is not allowed. `" + _.name + "` detected")

          def traversableTypes
            = descendats
                .filter(_ <:< Reflection[TraversableOnce[_]])
                .filterNot(r =>
                  //  using java class to erase generics
                  r.javaClass == classOf[Seq[_]] ||
                  r.javaClass == classOf[Set[_]] ||
                  r.javaClass == classOf[Map[_, _]] ||
                  r.javaClass == classOf[Range]
                )
                .map("Only general immutable `Seq`, `Set`, `Map` and `Range` are supported traversable types. `" + _ + "` detected instead")

          def inexistentPropertiesInKeys
            = ( e.indexed.toStream ++ e.unique )
                .flatten
                .filterNot(e.reflection.properties.contains)
                .map("Inexistent property: `" + _ + "`")

          def notDistinctPropertiesInKeys
            = ( e.indexed.toStream ++ e.unique )
                .filter(ps => ps.distinct.size != ps.size )
                .map("Not a distinct properties list: `" + _.mkString(", ") + "`")

          def recursiveEntities
            = {
              def checkType(r : Reflection, seen : Seq[Reflection]) : Option[String]
                = if( entityTypes.exists( _ =:= r ) && seen.exists( _ =:= r ) )
                    Some("Entity '" + e.reflection.toString + "' recurses at '" + r.toString + "'")
                  else if( seen.exists( _ =:= r ) )
                    None
                  else {
                    val types = r.properties.values.toStream ++ r.generics
                    val seen1 = r +: seen
                    types.flatMap(t => checkType(t, seen1)).headOption
                }
              checkType(e.reflection, Seq())
            }

          Stream() ++
          recursiveEntities ++
          containsId ++
          generalTypes ++
          traversableTypes ++
          inexistentPropertiesInKeys ++
          notDistinctPropertiesInKeys
        }

      entities.toStream.flatMap(validateEntity)
    }

  def initializeSchema ( mappings : Iterable[EntityMapping], connector : Connector, initMode : InitMode ) {
    initMode match {
      case InitMode.DropAllCreate =>
        connector.withConnection { connection =>
          try {
            connection.dropAllTables()
          } catch {
            case e : Throwable =>
              logger.warn("Couldn't drop all tables. " + e.getMessage)
          }
          mappings $ Create.tables foreach connection.createTable
        }
      case InitMode.DropCreate =>
        connector.withConnection { connection =>
          val tables = connection.listTables().toSet

          mappings $ Drop.tables map (_.name) filter tables.contains foreach { n =>
            try {
              connection.dropTable(n)
            } catch {
              case e : Throwable =>
                logger.warn("Couldn't drop table `" + n + "`. " + e.getMessage)
            }
          }
          mappings $ Create.tables foreach connection.createTable
        }
      case InitMode.Create =>
        connector.withConnection { connection =>
          val tables = connection.listTables().toSet

          mappings $ Create.tables filterNot { t => tables.contains(t.name) } foreach connection.createTable
        }
      case InitMode.DoNothing =>
    }
  }

}
