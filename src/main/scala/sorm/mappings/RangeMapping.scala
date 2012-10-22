package sorm.mappings

import sext._
import sorm._
import connection.Connection
import core._
import reflection._

class RangeMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends CompositeMapping
  {
    lazy val start = Mapping( Reflection[Int], Membership.RangeStart(this), settings, connection )
    lazy val end = Mapping( Reflection[Int], Membership.RangeEnd(this), settings, connection )
    lazy val mappings = start +: end +: Stream()

    def valueFromContainerRow(data: (String) => Any)
      = start.valueFromContainerRow(data).asInstanceOf[Int] to end.valueFromContainerRow(data).asInstanceOf[Int]
    def valuesForContainerTableRow(value: Any) = value match {
      case value : Range => start.valuesForContainerTableRow(value.start) ++ end.valuesForContainerTableRow(value.end)
    }
  }
