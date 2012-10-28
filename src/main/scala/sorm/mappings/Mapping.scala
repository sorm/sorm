package sorm.mappings

import sorm._
import driver.DriverConnection
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
          case Membership.OptionToTableItem(option) =>
            option.memberName
          case Membership.OptionToNullableItem(option) =>
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
  def valueFromContainerRow ( data : String => Any, connection : DriverConnection ) : Any

  //  for saving
  def valuesForContainerTableRow ( value : Any ) : Iterable[(String, Any)]
  def update ( value : Any, masterKey : Stream[Any], connection : DriverConnection ) {}
  def insert ( value : Any, masterKey : Stream[Any], connection : DriverConnection ) {}

  def columnsForContainer : Stream[Column]

  override def toString = reflection.name
}
object Mapping {
  def apply
    ( reflection : Reflection,
      membership : Option[Membership],
      settings : Map[Reflection, EntitySettings] )
    : Mapping
    = MappingKind( reflection ) match {
        case MappingKind.Value => 
          new ValueMapping( reflection, membership, settings )
        case MappingKind.Tuple => 
          new TupleMapping( reflection, membership, settings )
        case MappingKind.Seq => 
          new SeqMapping( reflection, membership, settings )
        case MappingKind.Set => 
          new SetMapping( reflection, membership, settings )
        case MappingKind.Map => 
          new MapMapping( reflection, membership, settings )
        case MappingKind.Entity => 
          new EntityMapping( reflection, membership, settings )
        case MappingKind.OptionToTable =>
          new OptionToTableMapping( reflection, membership, settings )
        case MappingKind.OptionToNullable =>
          new OptionToNullableMapping( reflection, membership, settings )
        case MappingKind.Enum =>
          new EnumMapping( reflection, membership, settings )
        case MappingKind.Range =>
          new RangeMapping( reflection, membership, settings )
      }

  def apply
    ( reflection : Reflection,
      membership : Membership,
      settings : Map[Reflection, EntitySettings] )
    : Mapping 
    = apply(reflection, Some(membership), settings)

}
