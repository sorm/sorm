package sorm.mappings

import sext.Sext._
import sorm._
import core._
import reflection.Reflection

class SetMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.SetItem(this), settings, connection )
    lazy val primaryKeyColumns = masterTableColumns :+ hashColumn
    lazy val hashColumn = ddl.Column( "h", ddl.Column.Type.Integer )
    lazy val mappings = item +: Stream()
    def parseRows ( rows : Stream[String => Any] )
      = rows.map(item.valueFromContainerRow).toSet

  }
