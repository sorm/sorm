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
 
    lazy val values
      = reflection.containerObject.get.asInstanceOf[Enumeration].values
          .toSeq.map( v => v.toString -> v ).toMap

    lazy val columnType
      = Column.Type.Enum(values.keys.toSeq)

    def autoIncremented = false
    
}