package vorm.structure

sealed trait Membership[ParentT] {
  def parent: ParentT
}

object Membership {
  sealed case class EntityProperty
    [ ParentT ]
    ( name : String, parent : ParentT )
    extends Membership[ParentT]
  sealed case class TupleItem
    [ ParentT ]
    ( index : Int, parent : ParentT )
    extends Membership[ParentT]
  sealed case class OptionItem
    [ ParentT ]
    ( parent : ParentT )
    extends Membership[ParentT]
  sealed case class SeqItem
    [ ParentT ]
    ( parent : ParentT )
    extends Membership[ParentT]
  sealed case class SetItem
    [ ParentT ]
    ( parent : ParentT )
    extends Membership[ParentT]
  sealed case class MapKey
    [ ParentT ]
    ( parent : ParentT )
    extends Membership[ParentT]
  sealed case class MapValue
    [ ParentT ]
    ( parent : ParentT )
    extends Membership[ParentT]
}