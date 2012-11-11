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
    lazy val dbValues : Map[Enumeration#Value, Short]
      = values.map(_.swap)
    private lazy val values : Map[Short, Enumeration#Value]
      = reflection.containerObject.get.asInstanceOf[Enumeration].values
          .view.map( v => v.id.toShort -> v ).toMap
    def columnType 
      = ColumnType.SmallInt
    def valueFromContainerRow ( data : String => Any, connection : DriverConnection )
      = data(memberName).asInstanceOf[Short] $ values
    def valuesForContainerTableRow( value : Any )
      = value.asInstanceOf[Enumeration#Value] $ dbValues $ (memberName -> _) $ (Stream(_))

  }