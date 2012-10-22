package sorm.mappings

import sext._
import sorm._
import connection.Connection
import reflection._
import ddl._
import core._

class EnumMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends ColumnMapping
  {
    lazy val dbValues : Map[Enumeration#Value, Byte]
      = values.map(_.swap)
    private lazy val values : Map[Byte, Enumeration#Value]
      = reflection.containerObject.get.asInstanceOf[Enumeration].values
          .view.map( v => v.id.toByte -> v ).toMap
    def columnType 
      = ColumnType.TinyInt
    def valueFromContainerRow ( data : String => Any )
      = data(memberName).asInstanceOf[Byte] as values
    def valuesForContainerTableRow( value : Any )
      = value.asInstanceOf[Enumeration#Value] $ dbValues $ (memberName -> _) $ (Stream(_))

  }