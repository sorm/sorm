package sorm.newMappings

import sext.Sext._

import sorm._
import reflection._
import core._

class TupleMapping 
  ( protected val reflection : Reflection, 
    protected val membership : Option[Membership], 
    protected val settings : Map[Reflection, EntitySettings],
    protected val driver : Driver ) 
  extends CompositeMapping 
  with Parsing {

  protected def mappings
    = reflection.generics.toStream.zipWithIndex.map { case (r, i) => Mapping(r, Membership.TupleItem(i, this), settings, driver) }
  protected def valueFromContainerRow ( row : Map[String, _], pk : Map[String, _] )
    = reflection instantiate mappings.map(_.valueFromContainerRow(row, pk))


}
