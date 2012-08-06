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
    lazy val item : Mapping
      = Mapping( Membership.SetItem(this), reflection.generics(0), settingsMap )

    lazy val children : Set[Mapping]
      = Set( item )

    lazy val primaryKeyColumns : IndexedSeq[Column]
      = containerTableColumns :+ hashColumn

    lazy val hashColumn : Column
      = Column( "h", Column.Type.Integer )

    lazy val columns : Set[Column]
      = primaryKeyColumns.view ++ childrenColumns.view toSet

  }
