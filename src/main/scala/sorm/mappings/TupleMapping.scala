package sorm.mappings

import sext.Sext._

import sorm._
import reflection._
import core._

class TupleMapping 
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends CompositeMapping {

  @inline def mappings
    = items.toStream

  lazy val items
    = reflection.generics.view.zipWithIndex.map { case (r, i) => Mapping(r, Membership.TupleItem(i, this), settings, driver) }.toVector

  def valueFromContainerRow ( row : String => Any )
    = reflection instantiate mappings.map(_.valueFromContainerRow(row))

  def valuesForContainerTableRow( value : Any )
    = mappings.zip(value.asInstanceOf[Product].productIterator.toIterable).flatMap{ case (m, v) => m.valuesForContainerTableRow(v) }

}
