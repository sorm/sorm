package sorm.mappings

import sorm._
import reflection.Reflection
import core._
import scala.Some
import ddl._

trait Mapping {
  def membership : Option[Membership]
  def reflection : Reflection
  def settings : Map[Reflection, EntitySettings]

  lazy val ancestors : Stream[Mapping]
    = membership.map(_.parent).map(p => p +: p.ancestors).getOrElse(Stream())
  lazy val root = (ancestors.lastOption getOrElse this).asInstanceOf[TableMapping]
  lazy val containerTableMapping : Option[TableMapping]
    = ancestors.collectFirst{ case c : TableMapping => c }
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
          case Membership.RangeStart(r) =>
            r.memberName + "$s"
          case Membership.RangeEnd(r) =>
            r.memberName + "$e"
        }
        .getOrElse("")


  //  for parsing
  def valueFromContainerRow ( data : String => Any ) : Any

  //  for saving
  def valuesForContainerTableRow ( value : Any ) : Iterable[(String, Any)]
  def update ( value : Any, masterKey : Stream[Any] ) {}
  def insert ( value : Any, masterKey : Stream[Any] ) {}

  def columnsForContainer : Stream[Column]
}
object Mapping {
  def apply
    ( reflection : Reflection,
      membership : Option[Membership],
      settings : Map[Reflection, EntitySettings],
      driver : Driver )
    : Mapping
    = MappingKind( reflection ) match {
        case MappingKind.Value => 
          new ValueMapping( reflection, membership, settings, driver )
        case MappingKind.Tuple => 
          new TupleMapping( reflection, membership, settings, driver )
        case MappingKind.Seq => 
          new SeqMapping( reflection, membership, settings, driver )
        case MappingKind.Set => 
          new SetMapping( reflection, membership, settings, driver )
        case MappingKind.Map => 
          new MapMapping( reflection, membership, settings, driver )
        case MappingKind.Entity => 
          new EntityMapping( reflection, membership, settings, driver )
        case MappingKind.Option => 
          new OptionMapping( reflection, membership, settings, driver )
        case MappingKind.Enum =>
          new EnumMapping( reflection, membership, settings, driver )
        case MappingKind.Range =>
          new RangeMapping( reflection, membership, settings, driver )
      }

  def apply
    ( reflection : Reflection,
      membership : Membership,
      settings : Map[Reflection, EntitySettings],
      driver : Driver )
    : Mapping 
    = apply(reflection, Some(membership), settings, driver)

}
