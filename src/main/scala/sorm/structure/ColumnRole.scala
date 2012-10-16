package sorm.structure

import mapping._

sealed trait ColumnRole {
  def mapping : Mapping
  def jdbcType : Int
}
object ColumnRole {

  sealed case class GeneratedId
    ( jdbcType : Int, 
      mapping : Mapping )
    extends ColumnRole 

  sealed case class Value
    ( jdbcType : Int, 
      mapping : Mapping )
    extends ColumnRole

  sealed case class ForeignKeyPart
    ( jdbcType : Int, 
      index : Int,
      mapping : Mapping )
    extends ColumnRole

  sealed case class Index
    ( jdbcType : Int, 
      mapping : Mapping )
    extends ColumnRole

  sealed case class Hash
    ( jdbcType : Int, 
      mapping : Mapping )
    extends ColumnRole

}
