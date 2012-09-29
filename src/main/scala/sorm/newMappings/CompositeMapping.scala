package sorm.newMappings

import sext.Sext._

import sorm._
import reflection.Reflection
import core._
import ddl._

trait CompositeMapping extends Mapping {
  def mappings : Stream[Mapping]
  lazy val columns : Stream[Column]
    = mappings flatMap {
        case m : TableMapping => Stream()
        case m : CompositeMapping => m.columns
        case m : ColumnMapping => m.column +: Stream()
      }

  /**
   * First descendant table mappings
   * Or containedTableMappings
   */
  lazy val containedTableMappings : Stream[TableMapping]
    = mappings flatMap {
        case m : ColumnMapping => Stream()
        case m : TableMapping => m +: Stream()
        case m : CompositeMapping => m.containedTableMappings
      }
}


