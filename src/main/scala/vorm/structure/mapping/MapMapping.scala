package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

sealed class MapMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends CollectionTableMapping
  {
    lazy val children
      = key :: value :: Nil
    lazy val key
      = Mapping( Membership.MapKey(this), reflection.generics(0), settingsMap )
    lazy val value
      = Mapping( Membership.MapValue(this), reflection.generics(1), settingsMap )

    lazy val primaryKeyColumns
      = ownerTable.get.primaryKeyColumns
          .map{ c => c.copy(name = "p_" + c.name, autoIncremented = false) } :+
        Column("h", Column.Type.Integer)

//
//    lazy val keyForeignKeys
//      = key.ownerTableForeignKeys
//    lazy val valueForeignKeys
//      = value.ownerTableForeignKeys
//    lazy val foreignKeys
//      = parentForeignKey ++: keyForeignKeys ::: valueForeignKeys

  }
