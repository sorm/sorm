package vorm.structure

sealed trait ColumnRole {
  def mapping: Mapping
}
object ColumnRole {

  sealed case class GeneratedId
    ( mapping : Mapping )
    extends ColumnRole

  sealed case class Value
    ( mapping : Mapping )
    extends ColumnRole

  sealed case class ForeignKeyPart
    ( index : Int,
      mapping : Mapping )
    extends ColumnRole

  sealed case class Index
    ( mapping : Mapping )
    extends ColumnRole

  sealed case class Hash
    ( mapping : Mapping )
    extends ColumnRole

}
