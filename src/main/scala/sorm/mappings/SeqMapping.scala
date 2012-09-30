package sorm.mappings

import sext.Sext._
import sorm._
import core._
import reflection.Reflection

class SeqMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.SeqItem(this), settings, connection )
    lazy val primaryKeyColumns = masterTableColumns :+ indexColumn
    lazy val indexColumn = ddl.Column( "i", ddl.Column.Type.Integer )
    lazy val mappings = item +: Stream()
    def parseRows ( rows : Stream[String => Any] )
      = rows.map(item.valueFromContainerRow).toVector

  }