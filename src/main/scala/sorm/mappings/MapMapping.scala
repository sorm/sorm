package sorm.mappings

import sext.Sext._
import sorm._
import core._
import reflection.Reflection

sealed class MapMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends SlaveTableMapping
  {

    lazy val key = Mapping( reflection.generics(0), Membership.MapKey(this), settings, connection )
    lazy val value = Mapping( reflection.generics(1), Membership.MapValue(this), settings, connection )
    lazy val primaryKeyColumns = masterTableColumns :+ hashColumn
    lazy val hashColumn = ddl.Column( "h", ddl.Column.Type.Integer )
    lazy val mappings = key +: value +: Stream()
    def parseRows ( rows : Stream[String => Any] )
      = rows.map(r => key.valueFromContainerRow(r) -> value.valueFromContainerRow(r)).toMap

  }