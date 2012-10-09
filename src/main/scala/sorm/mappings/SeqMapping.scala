package sorm.mappings

import sext.Sext._
import sorm._
import core._
import reflection.Reflection

class SeqMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.SeqItem(this), settings, driver )
    lazy val index = new ValueMapping( Reflection[Int], Some(Membership.SeqIndex(this)), settings, driver )
    lazy val primaryKeyColumns = masterTableColumns :+ index.column
    lazy val mappings = item +: Stream()
    def parseRows ( rows : Stream[String => Any] )
      = rows.map(item.valueFromContainerRow).toVector

  }