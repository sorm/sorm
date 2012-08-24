package sorm.structure.mapping

import sorm._
import extensions._
import reflection._
import ddl._
import structure._

sealed class MapMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends CollectionMapping
  {
    lazy val children
      = Set() + key + value

    lazy val key
      = Mapping( Membership.MapKey(this), reflection.generics(0), settingsMap )

    lazy val value
      = Mapping( Membership.MapValue(this), reflection.generics(1), settingsMap )

    lazy val primaryKeyColumns : IndexedSeq[Column]
      = containerTableColumns :+ hashColumn

    lazy val hashColumn : Column
      = Column( "h", Column.Type.Integer )

    lazy val columns : Set[Column]
      = primaryKeyColumns.view ++ childrenColumns.view toSet


  }
