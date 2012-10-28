package sorm.mappings

import sext._, embrace._
import sorm._
import driver.DriverConnection
import reflection._
import ddl._
import core._

class EnumMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings] )
  extends ColumnMapping
  {
    lazy val dbValues : Map[Enumeration#Value, Byte]
      = values.map(_.swap)
    private lazy val values : Map[Byte, Enumeration#Value]
      = reflection.containerObject.get.asInstanceOf[Enumeration].values
          .view.map( v => v.id.toByte -> v ).toMap
    def columnType 
      = ColumnType.TinyInt
    def valueFromContainerRow ( data : String => Any, connection : DriverConnection )
      = data(memberName).asInstanceOf[Byte] $ values
    def valuesForContainerTableRow( value : Any )
      = value.asInstanceOf[Enumeration#Value] $ dbValues $ (memberName -> _) $ (Stream(_))

  }