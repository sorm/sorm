package sorm.mappings


sealed trait Membership {
  def parent: Mapping
}

object Membership {

  sealed case class RangeStart
    ( parent : RangeMapping )
    extends Membership
  sealed case class RangeEnd
    ( parent : RangeMapping )
    extends Membership
  sealed case class SeqIndex
    ( parent : SeqMapping )
    extends Membership
  sealed case class EntityId
    ( parent : EntityMapping )
    extends Membership
  sealed case class EntityProperty
    ( name : String, parent : EntityMapping )
    extends Membership
  sealed case class TupleItem
    ( index : Int, parent : TupleMapping )
    extends Membership
  sealed case class OptionToTableItem
    ( parent : OptionToTableMapping )
    extends Membership
  sealed case class OptionToNullableItem
    ( parent : OptionToNullableMapping )
    extends Membership
  sealed case class SeqItem
    ( parent : SeqMapping )
    extends Membership
  sealed case class SetItem
    ( parent : SetMapping )
    extends Membership
  sealed case class MapKey
    ( parent : MapMapping )
    extends Membership
  sealed case class MapValue
    ( parent : MapMapping )
    extends Membership

}
