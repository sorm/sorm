package sorm.structure.mapping

import sorm._
import sext.Sext._
import reflection._
import ddl._
import structure._

sealed class OptionMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends Mapping
  with HasChildren
  {
    def children = item :: Nil
    lazy val item
      = Mapping( Membership.OptionItem(this), 
                 reflection.generics(0), 
                 settingsMap )

    lazy val columns
      = columnsForContainerTable(item)

  }
