package vorm.structure.mapping

import vorm._
import structure._
import reflection._

trait Table
  extends Mapping
  {
    lazy val primaryKeyColumns
      : scala.Seq[ddl.Column]
      = ???
    lazy val resultSetColumns
      : scala.Seq[ddl.Column]
      = ???
    lazy val allColumns
      : scala.Seq[ddl.Column]
      = ???
    lazy val valueMappings
      : scala.Seq[Mapping]
      = ???
    lazy val subTableMappings
      : scala.Seq[Table]
      = ???

    lazy val tableName
      : String
      = ???

    lazy val ownerTableColumnMappings
      : scala.Seq[(String, String)]
      = ???

    lazy val ownerTableMapping
      : Table
      = ???
  }