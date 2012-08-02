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
    lazy val primaryKey
      = ownerTable.get.primaryKey.view.map("p_" + _) :+ "h" toList
  }
