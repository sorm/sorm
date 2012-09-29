package sorm.newMappings

import sext.Sext._

import sorm._
import core._
import reflection._
import ddl._

sealed class OptionMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Connection )
  extends CompositeMapping
  {

  def mappings = item +: Stream()

  lazy val item = Mapping( reflection.generics(0), Membership.OptionItem(this), settings, driver )

  override def valueFromContainerRow(data: String => Any) = item.memberName as data as Option.apply

}
