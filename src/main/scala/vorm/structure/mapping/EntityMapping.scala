package vorm.structure.mapping

import vorm._
import reflection._
import ddl._
import select._
import structure._
import extensions._

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
              Mapping (
                Membership.EntityProperty( name, this ),
                reflection,
                settingsMap
              )
          }

    lazy val settings = settingsMap(reflection)

    def columns = childrenColumns

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

    lazy val generatedIdColumn
      = Column("id", Column.Type.BigInt, autoIncrement = true)
}
