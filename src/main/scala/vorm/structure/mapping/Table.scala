package vorm.structure.mapping

import vorm._
import structure._
import reflection._

trait Table
  extends Mapping
  with HasChildren
  {
    lazy val primaryKeyColumns
      : scala.Seq[ddl.Column]
      = ???
    lazy val resultSetColumns
      : scala.Seq[ddl.Column]
      = ???
    lazy val subTableMappings
      : scala.Seq[Table]
      = ???
  }