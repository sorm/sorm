package sorm.mappings

import sext.Sext._
import sorm._
import core._
import reflection._

sealed class RangeMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends CompositeMapping
  {
    lazy val from = Mapping( Reflection[Int], Membership.RangeFrom(this), settings, connection )
    lazy val to = Mapping( Reflection[Int], Membership.RangeTo(this), settings, connection )
    lazy val mappings = from +: to +: Stream()

    def valueFromContainerRow(data: (String) => Any)
      = from.valueFromContainerRow(data).asInstanceOf[Int] to to.valueFromContainerRow(data).asInstanceOf[Int]
}
