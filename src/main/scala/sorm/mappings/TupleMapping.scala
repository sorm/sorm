package sorm.mappings

import sext.Sext._

import sorm._
import reflection._
import core._

class TupleMapping 
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends CompositeMapping {

  def mappings
    = reflection.generics.toStream.zipWithIndex.map { case (r, i) => Mapping(r, Membership.TupleItem(i, this), settings, connection) }

  override def valueFromContainerRow ( row : String => Any )
    = reflection instantiate mappings.map(_.valueFromContainerRow(row))

}
