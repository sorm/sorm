package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

trait Mapping {
  def membership : Option[Membership]
  def reflection : Reflection

  def parent = membership map {_.parent}

  override def toString : String
    = membership
        .map{
          case Membership.SeqIndex(_)
            ⇒ "seqIndex"
          case Membership.EntityId(_)
            ⇒ "id"
          case Membership.EntityProperty(name, _)
            ⇒ name
          case Membership.TupleItem(index, tuple)
            ⇒ "_" + index
          case Membership.OptionItem(option)
            ⇒ "optionItem"
          case Membership.SeqItem(_)
            ⇒ "seqItem"
          case Membership.SetItem(_)
            ⇒ "setItem"
          case Membership.MapKey(_)
            ⇒ "mapKey"
          case Membership.MapValue(_)
            ⇒ "mapValue"
        }
        .map{ membership.get.parent.toString + "." + _ }
        .getOrElse( reflection.name )

  lazy val columnName : String
    = membership
        .map{
          case Membership.SeqIndex(_)
            ⇒ "i"
          case Membership.EntityId(_)
            ⇒ "id"
          case Membership.EntityProperty(name, _)
            ⇒ name.asDdlName
          case Membership.TupleItem(index, tuple)
            ⇒ tuple.columnName + "$" + index
          case Membership.OptionItem(option)
            ⇒ option.columnName
          case Membership.SeqItem(_)
            ⇒ "v"
          case Membership.SetItem(_)
            ⇒ "v"
          case Membership.MapKey(_)
            ⇒ "k"
          case Membership.MapValue(_)
            ⇒ "v"
        }
        .getOrElse("")

  lazy val tableName : String
    = this match {
        case _ : EntityMapping 
          ⇒ reflection.name.asDdlName
        case _
          ⇒ membership
              .map {
                case Membership.EntityProperty(name, entity)
                  ⇒ entity.tableName + "$" + name.asDdlName
                case Membership.TupleItem(index, tuple)
                  ⇒ tuple.tableName + "$" + index
                case Membership.SeqItem(seq)
                  ⇒ seq.tableName + "$v"
                case Membership.SetItem(set)
                  ⇒ set.tableName + "$v"
                case Membership.MapKey(map)
                  ⇒ map.tableName + "$k"
                case Membership.MapValue(map)
                  ⇒ map.tableName + "$v"
              }
              .getOrElse( reflection.name.asDdlName )
      }

  lazy val containerTableMapping : Option[TableMapping]
    = membership
        .map{ _.parent }
        .flatMap{
          case parent : TableMapping
            ⇒ Some(parent)
          case parent : Mapping
            ⇒ parent.containerTableMapping
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
    = ReflectionKind( reflection ) match {
        case ReflectionKind.Value
          ⇒ new ValueMapping (membership, reflection, settings)
        case ReflectionKind.Tuple
          ⇒ new TupleMapping (membership, reflection, settings)
        case ReflectionKind.Seq
          ⇒ new SeqMapping (membership, reflection, settings)
        case ReflectionKind.Set
          ⇒ new SetMapping (membership, reflection, settings)
        case ReflectionKind.Map
          ⇒ new MapMapping (membership, reflection, settings)
        case ReflectionKind.Entity
          ⇒ new EntityMapping (membership, reflection, settings)
        case ReflectionKind.Option
          ⇒ new OptionMapping (membership, reflection, settings)
        case ReflectionKind.Entity
          ⇒ new EntityMapping (membership, reflection, settings)
      }
}