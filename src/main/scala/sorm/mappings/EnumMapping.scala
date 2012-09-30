package sorm.mappings

import sext.Sext._
import sorm._
import reflection._
import ddl._
import core._

sealed class EnumMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends ColumnMapping
  {
    private lazy val name = reflection.containerObjectName.get
    private lazy val dbValues : Map[Enumeration#Value, Byte]
      = values.map(_.swap)
    private lazy val values : Map[Byte, Enumeration#Value]
      = reflection.containerObject.get.asInstanceOf[Enumeration].values
          .view.map( v => v.id.toByte -> v ).toMap
    def columnType = Column.Type.TinyInt
    def valueFromContainerRow(data: String => Any)
      = data(memberName).asInstanceOf[Byte] as values
  }