package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

sealed class SetMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends CollectionTableMapping
  {
    lazy val item
      = Mapping( Membership.SetItem(this), reflection.generics(0), settingsMap )

    lazy val primaryKeyColumns
      = ownerTable.get.primaryKeyColumns
          .map{ c => c.copy(name = "p_" + c.name, autoIncremented = false) } :+
        Column("h", Column.Type.Integer)
  }
