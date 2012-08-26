package sorm.structure.mapping

import sorm._
import extensions._
import reflection._
import ddl._
import structure._

sealed class EnumMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends ColumnMapping
  {

    lazy val name
      = reflection.containerObjectName.get
 
    lazy val dbValues : Map[Enumeration#Value, Byte]
      = values.map(_.swap)

    lazy val values : Map[Byte, Enumeration#Value]
      = reflection.containerObject.get.asInstanceOf[Enumeration].values
          .view.map( v => v.id.toByte -> v ).toMap

    lazy val columnType
      = Column.Type.TinyInt

    def autoIncremented = false
    
}