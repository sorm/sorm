package sorm.mappings

import sext.Sext._
import sorm._
import core._
import reflection._

sealed class OptionMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.OptionItem(this), settings, driver )
    lazy val primaryKeyColumns = masterTableColumns
    lazy val mappings = item +: Stream()
    def parseRows ( rows : Stream[String => Any] )
      = rows.headOption.map(item.valueFromContainerRow)
  }
