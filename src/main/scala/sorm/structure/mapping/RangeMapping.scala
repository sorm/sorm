package sorm.structure.mapping

import sorm._
import reflection._
import structure._

sealed class RangeMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends Mapping
  with HasChildren
  {
    def children = Vector(from, to)

    lazy val from
      = Mapping(Membership.RangeFrom(this), Reflection[Int], settingsMap) 
    lazy val to
      = Mapping(Membership.RangeTo(this), Reflection[Int], settingsMap)

  }
