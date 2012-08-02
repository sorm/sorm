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

  override def toString
    : String
    = membership.map(_.parent).map(_.toString + ".").getOrElse("") +
      reflection.name

  lazy val columnName
    : String
    = membership
        .map{
          case Membership.EntityProperty(name, _)
            ⇒ name.asDdlName
          case Membership.TupleItem(index, tuple)
            ⇒ tuple.columnName + "_" + index
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

  lazy val tableName
    : String
    = this match {
        case _ : EntityMapping 
          ⇒ reflection.name.asDdlName
        case _
          ⇒ membership
              .map {
                case Membership.EntityProperty(name, entity)
                  ⇒ entity.tableName + "_" + name.asDdlName
                case Membership.TupleItem(index, tuple)
                  ⇒ tuple.tableName + "_" + index
                case Membership.SeqItem(seq)
                  ⇒ seq.tableName + "_v"
                case Membership.SetItem(set)
                  ⇒ set.tableName + "_v"
                case Membership.MapKey(map)
                  ⇒ map.tableName + "_k"
                case Membership.MapValue(map)
                  ⇒ map.tableName + "_v"
              }
              .getOrElse( reflection.name.asDdlName )
      }

  lazy val ownerTable
    : Option[TableMapping]
    = membership
        .map(_.parent)
        .flatMap {
          case parent : TableMapping
            ⇒ Some(parent)
          case parent : Mapping
            ⇒ parent.ownerTable
        }

//  lazy val ownerTableForeignKeys
//    : List[ForeignKey]
//    = Nil
//
//  lazy val parentForeignKey
//    = ownerTable map {
//        ownerTable
//          ⇒ ForeignKey (
//              ownerTable.tableName,
//              ownerTable.primaryKey.map(n => "p_" + n -> n).toSet,
//              ForeignKey.ReferenceOption.Cascade
//            )
//      }

}
object Mapping 
  {
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
