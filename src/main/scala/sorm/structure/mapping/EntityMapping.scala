package sorm.structure.mapping

import sorm._
import reflection._
import ddl._
import structure._
import extensions.Extensions._

sealed class EntityMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends TableMapping
  {
    lazy val children
      = properties.values

    lazy val properties : Map[String, Mapping]
      = reflection.properties
          .map{
            case (name, reflection) ⇒
              name →
              Mapping(
                Membership.EntityProperty( name, this ),
                reflection,
                settingsMap
              )
          }
    lazy val id
      = new ValueMapping(Some(Membership.EntityId(this)), Reflection[Long], settingsMap)

    lazy val settings = settingsMap(reflection)

    lazy val columns
      = Set.empty +
        generatedIdColumn ++
        childrenColumns

    lazy val primaryKeyColumns
      = generatedIdColumn :: Nil

    lazy val uniqueKeyColumns
      = settings.uniqueKeys
          .view
          .map{
            _.view
              .map{ properties }
              .flatMap{ columnsForContainerTable }
              .toSeq
          }
          .toSet

    lazy val indexColumns
      = settings.indexes
          .view
          .map{
            _.view
              .map{ properties }
              .flatMap{ columnsForContainerTable }
              .toSeq
          }
          .toSet

    def foreignKeys
      = nestedTableMappingsForeignKeys

    def generatedIdColumn : Column
      = id.column

    lazy val bindingsToContainerTable : Seq[(String, String)]
      = containerTableMapping
          .get.foreignKeys(this)
          .bindings.view.map{_.swap}
  }
