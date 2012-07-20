package vorm.structure.mapping

import vorm._
import structure._
import reflection._

trait Table
  extends Mapping
  with HasChildren
  {
    lazy val primaryKeyColumnRoles
      : scala.Seq[ColumnRole]
      = ???
    lazy val resultSetColumnRoles
      : scala.Seq[ColumnRole]
      = ???
    lazy val subTableMappings
      : scala.Seq[Table]
      = ???
  }