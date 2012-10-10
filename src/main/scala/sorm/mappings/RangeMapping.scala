package sorm.mappings

import sext.Sext._
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
    lazy val from = Mapping( Reflection[Int], Membership.RangeFrom(this), settings, driver )
    lazy val to = Mapping( Reflection[Int], Membership.RangeTo(this), settings, driver )
    lazy val mappings = from +: to +: Stream()

    def valueFromContainerRow(data: (String) => Any)
      = from.valueFromContainerRow(data).asInstanceOf[Int] to to.valueFromContainerRow(data).asInstanceOf[Int]
    def valuesForContainerTableRow(value: Any) = value match {
      case value : Range => from.valuesForContainerTableRow(value.start) ++ to.valuesForContainerTableRow(value.end)
    }
  }
