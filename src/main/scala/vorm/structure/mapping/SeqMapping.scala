package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

sealed class SeqMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends CollectionMapping
  { 
    lazy val item : Mapping
      = Mapping( Membership.SeqItem(this), reflection.generics(0), settingsMap )

    lazy val children : Set[Mapping]
      = Set( item )

    lazy val index
      = new ValueMapping(Some(Membership.SeqIndex(this)), Reflection[Int], settingsMap)

    lazy val primaryKeyColumns : IndexedSeq[Column]
      = containerTableColumns :+ indexColumn

    def indexColumn : Column
      = index.column

    lazy val columns : Set[Column]
      = primaryKeyColumns.view ++ childrenColumns.view toSet

  }