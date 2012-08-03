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
    lazy val properties
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

    lazy val columns = valueColumns

    lazy val primaryKeyColumns
      = settings.primaryKey.view.map{ properties }.flatMap{ columnsForOwner }.toList

    lazy val foreignKeyForOwnerTable
      = membership map { _ =>
          ForeignKey(
            tableName,
            primaryKeyColumns.map{c ⇒ (columnName + "_" + c.name) → c.name},
            ForeignKey.ReferenceOption.Cascade
          )
        }


    def ownerTableForeignKey = None

//
//    lazy val generatedIdColumn
//      : Option[Column]
//      = if (settingsMap.primaryKey contains reflection)
//          None
//        else
//          Some (
//            Column (
//              "id",
//              ColumnType.BigInt,
//              autoIncremented = true,
//              nullable = false
//            )
//          )
//    private def keyColumns
//      ( propertyNames : Seq[String] )
//      = propertyNames.view
//          .map(properties)
//          .flatMap {
//            case property : ValueMapping
//              ⇒ property.columnName :: Nil
//            case property : TupleMapping
//              ⇒ property.items.map(_.columnName)
//            case property : OptionMapping
//              ⇒ property.item.columnName :: Nil
//            case _
//              ⇒ Nil
//          }
//          .toList
}
