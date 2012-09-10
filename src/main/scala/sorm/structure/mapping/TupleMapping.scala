package sorm.structure.mapping

import sorm._
import sext.Sext._
import reflection._
import ddl._
import structure._

sealed class TupleMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends Mapping
  with HasChildren
  {
    def children = items

    lazy val items
      = reflection.generics.view.zipWithIndex
          .map {
            case (r, i)
              ⇒ Mapping(Membership.TupleItem(i, this), r, settingsMap)
          }
          .toIndexedSeq

  }
