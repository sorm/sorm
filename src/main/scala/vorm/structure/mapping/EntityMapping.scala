package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

sealed class EntityMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends TableMapping
  {
    lazy val properties
      = reflection.properties
          .map {
            case (name, reflection)
              ⇒ name →
                Mapping (
                  Membership.EntityProperty( name, this ),
                  reflection,
                  settingsMap
                )
          }

    lazy val settings = settingsMap(reflection)

    def primaryKey = settings.primaryKey
    def uniqueKeys = settings.uniqueKeys
    def indexes = settings.indexes
    def autoIncrement = settings.autoIncrement

    lazy val columns = properties.values flatMap subColumns
    
//    override lazy val ownerTableColumns
//      = primaryKeyColumns.view
//          .map(_.column)
//          .map(
//            c ⇒ c copy (
//                  name = columnName + "_" + c.name,
//                  autoIncremented = false
//                )
//          )
//          // .map(MappedColumn.ForeignKeyPart)
//
//    override lazy val ownerTableForeignKeys
//      = membership map {
//          ForeignKey (
//            tableName,
//            primaryKey.map(c ⇒ columnName + "_" + c → c),
//            ForeignKey.ReferenceOption.Cascade
//          )
//        } toList
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
