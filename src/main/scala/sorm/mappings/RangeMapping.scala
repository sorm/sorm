package sorm.mappings

import sext._, embrace._
import sorm._
import driver.DriverConnection
import core._
import reflection._

class RangeMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings] )
  extends CompositeMapping
  {
    lazy val start = Mapping( Reflection[Int], Membership.RangeStart(this), settings )
    lazy val end = Mapping( Reflection[Int], Membership.RangeEnd(this), settings )
    lazy val mappings = start +: end +: Stream()

    def valueFromContainerRow(data: String => Any, c : DriverConnection)
      = start.valueFromContainerRow(data, c).asInstanceOf[Int] to end.valueFromContainerRow(data, c).asInstanceOf[Int]
    def valuesForContainerTableRow(value: Any) = value match {
      case value : Range => start.valuesForContainerTableRow(value.start) ++ end.valuesForContainerTableRow(value.end)
    }
  }
