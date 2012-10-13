package sorm.mappings

import sext._
import sorm._
import core._
import reflection._

class RangeMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends CompositeMapping
  {
    lazy val start = Mapping( Reflection[Int], Membership.RangeStart(this), settings, driver )
    lazy val end = Mapping( Reflection[Int], Membership.RangeEnd(this), settings, driver )
    lazy val mappings = start +: end +: Stream()

    def valueFromContainerRow(data: (String) => Any)
      = start.valueFromContainerRow(data).asInstanceOf[Int] to end.valueFromContainerRow(data).asInstanceOf[Int]
    def valuesForContainerTableRow(value: Any) = value match {
      case value : Range => start.valuesForContainerTableRow(value.start) ++ end.valuesForContainerTableRow(value.end)
    }
  }
