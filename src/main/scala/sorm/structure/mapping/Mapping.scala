package sorm.structure.mapping

import sorm._
import extensions.Extensions._
import reflection._
import ddl._
import structure._

trait Mapping {
  def membership : Option[Membership]
  def reflection : Reflection

  def parent = membership map {_.parent}
  lazy val root : TableMapping
    = parent map {_.root} getOrElse asInstanceOf[TableMapping]

  override def toString : String
    = membership
        .map{
          case Membership.SeqIndex(_) => 
            "seqIndex"
          case Membership.EntityId(_) => 
            "id"
          case Membership.EntityProperty(name, _) => 
            name
          case Membership.TupleItem(index, tuple) => 
            "_" + index
          case Membership.OptionItem(option) => 
            "optionItem"
          case Membership.SeqItem(_) => 
            "seqItem"
          case Membership.SetItem(_) => 
            "setItem"
          case Membership.MapKey(_) => 
            "mapKey"
          case Membership.MapValue(_) => 
            "mapValue"
          case Membership.RangeFrom(_) =>
            "rangeFrom"
          case Membership.RangeTo(_) =>
            "rangeTo"
        }
        .map{ membership.get.parent.toString + "." + _ }
        .getOrElse( reflection.name )

  lazy val columnName : String
    = membership
        .map{
          case Membership.SeqIndex(_) => 
            "i"
          case Membership.EntityId(_) => 
            "id"
          case Membership.EntityProperty(name, _) => 
            ddlName(name)
          case Membership.TupleItem(index, tuple) => 
            tuple.columnName + "$" + index
          case Membership.OptionItem(option) => 
            option.columnName
          case Membership.SeqItem(_) => 
            "v"
          case Membership.SetItem(_) => 
            "v"
          case Membership.MapKey(_) => 
            "k"
          case Membership.MapValue(_) => 
            "v"
          case Membership.RangeFrom(_) =>
            "f"
          case Membership.RangeTo(_) =>
            "t"
        }
        .getOrElse("")

  lazy val tableName : String
    = this match {
        case _ : EntityMapping => ddlName(reflection.name)
        case _ => 
          membership
            .map {
              case Membership.EntityProperty(name, entity) => 
                entity.tableName + "$" + ddlName(name)
              case Membership.TupleItem(index, tuple) => 
                tuple.tableName + "$" + index
              case Membership.MapKey(map) => 
                map.tableName + "$k"
              case _ => 
                parent.get.tableName + "$v"
            }
            .getOrElse( ddlName(reflection.name) )
      }

  lazy val containerTableMapping : Option[TableMapping]
    = membership
        .map{ _.parent }
        .flatMap{
          case parent : TableMapping => Some(parent)
          case parent : Mapping => parent.containerTableMapping
        }

}
object Mapping {
  def apply
    ( membership : Membership,
      reflection : Reflection,
      settings : SettingsMap )
    : Mapping
    = apply(Some(membership), reflection, settings)
    
  def apply
    ( membership : Option[Membership],
      reflection : Reflection,
      settings : SettingsMap )
    : Mapping
    = MappingKind( reflection ) match {
        case MappingKind.Value => 
          new ValueMapping( membership, reflection, settings )
        case MappingKind.Tuple => 
          new TupleMapping( membership, reflection, settings )
        case MappingKind.Seq => 
          new SeqMapping( membership, reflection, settings )
        case MappingKind.Set => 
          new SetMapping( membership, reflection, settings )
        case MappingKind.Map => 
          new MapMapping( membership, reflection, settings )
        case MappingKind.Entity => 
          new EntityMapping( membership, reflection, settings )
        case MappingKind.Option => 
          new OptionMapping( membership, reflection, settings )
        case MappingKind.Enum =>
          new EnumMapping( membership, reflection, settings )
        case MappingKind.Range =>
          new RangeMapping( membership, reflection, settings )
      }
}