package sorm.mappings

import sext.Sext._

import sorm._
import reflection.Reflection
import core._
import scala.Some
import ddl._

trait Mapping extends Parsing {
  lazy val containerTableMapping : Option[TableMapping]
    = membership
        .map{ _.parent }
        .flatMap{
          case parent : TableMapping => Some(parent)
          case parent : Mapping => parent.containerTableMapping
        }
  def membership : Option[Membership]
  def reflection : Reflection
  def settings : Map[Reflection, EntitySettings]

//  lazy val ancestors = this.unfold1( _.membership.map(_.parent) )
  lazy val ancestors : Stream[Mapping]
    = membership.map(_.parent).map(p => p +: p.ancestors).getOrElse(Stream())

  lazy val memberName : String
    = membership
        .map{
          case Membership.SeqIndex(_) =>
            "i"
          case Membership.EntityId(_) =>
            "id"
          case Membership.EntityProperty(name, _) =>
            ddlName(name)
          case Membership.TupleItem(index, tuple) =>
            tuple.memberName + "$" + index
          case Membership.OptionItem(option) =>
            option.memberName
          case Membership.SeqItem(_) =>
            "v"
          case Membership.SetItem(_) =>
            "v"
          case Membership.MapKey(_) =>
            "k"
          case Membership.MapValue(_) =>
            "v"
          case Membership.RangeFrom(r) =>
            r.memberName + "$f"
          case Membership.RangeTo(r) =>
            r.memberName + "$t"
        }
        .getOrElse("")
}
object Mapping {
  def apply
    ( reflection : Reflection,
      membership : Option[Membership],
      settings : Map[Reflection, EntitySettings],
      driver : Driver )
    : Mapping
    = ???
  def apply
    ( reflection : Reflection,
      membership : Membership,
      settings : Map[Reflection, EntitySettings],
      driver : Driver )
    : Mapping 
    = ???
}
