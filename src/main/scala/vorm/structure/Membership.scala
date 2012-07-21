package vorm.structure

sealed trait Membership {
  def parent: Mapping
}

object Membership
  {
    sealed case class EntityProperty
      ( name : String, parent : Mapping )
      extends Membership
    sealed case class TupleItem
      ( index : Int, parent : Mapping )
      extends Membership
    sealed case class OptionItem
      ( parent : Mapping )
      extends Membership
    sealed case class SeqItem
      ( parent : Mapping )
      extends Membership
    sealed case class SetItem
      ( parent : Mapping )
      extends Membership
    sealed case class MapKey
      ( parent : Mapping )
      extends Membership
    sealed case class MapValue
      ( parent : Mapping )
      extends Membership
  }