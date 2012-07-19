package vorm.structure

sealed trait ColumnRole {
  def mapping: Mapping
}
object ColumnRole {

  //  Entity columns:
  sealed case class GeneratedId
    ( mapping : Mapping )
    extends ColumnRole

  //  Value:
  sealed case class Value
    ( mapping : Mapping )
    extends ColumnRole

  //  Tuple:
  sealed case class TupleItemKeyPart
    ( index : Int,
      tupleItemIndex : Int,
      mapping : Mapping )
    extends ColumnRole

  //  Option, Seq, Set:
  sealed case class ItemKeyPart
    ( index : Int,
      mapping : Mapping )
    extends ColumnRole

  //  for all collections
  sealed case class ParentKeyPart
    ( index : Int, 
      mapping : Mapping )
    extends ColumnRole

  //  Seq:
  sealed case class Index
    ( mapping : Mapping )
    extends ColumnRole

  //  Hashed collections:
  sealed case class Hash
    ( mapping : Mapping )
    extends ColumnRole

  sealed case class MapKeyKeyPart
    ( index : Int, 
      mapping : Mapping )
    extends ColumnRole

  sealed case class MapValueKeyPart
    ( index : Int, 
      mapping : Mapping )
    extends ColumnRole


}
