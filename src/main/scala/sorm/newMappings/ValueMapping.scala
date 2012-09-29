package sorm.newMappings

import sorm._
import core._
import reflection._
import ddl._
import org.joda.time._

class ValueMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends ColumnMapping {


}