package sorm.newMappings


sealed trait Membership {
  def parent: Mapping
}

object Membership {

//  sealed case class RangeFrom
//    ( parent : RangeMapping )
//    extends Membership
//  sealed case class RangeTo
//    ( parent : RangeMapping )
//    extends Membership
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
//  sealed case class OptionItem
//    ( parent : OptionMapping )
//    extends Membership
  sealed case class SeqItem
    ( parent : SeqMapping )
    extends Membership
//  sealed case class SetItem
//    ( parent : SetMapping )
//    extends Membership
//  sealed case class MapKey
//    ( parent : MapMapping )
//    extends Membership
//  sealed case class MapValue
//    ( parent : MapMapping )
//    extends Membership

}
