package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
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
