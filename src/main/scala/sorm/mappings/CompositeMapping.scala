package sorm.mappings

import sext.Sext._

import sorm._
import reflection.Reflection
import core._
import ddl._

trait CompositeMapping extends Mapping {

  def mappings : Stream[Mapping]

  lazy val compositeColumns : Stream[Column]
    = mappings flatMap {
        case m : MasterTableMapping => m.columnsForContainer
        case m : SlaveTableMapping => Stream()
        case m : CompositeMapping => m.columns
        case m : ColumnMapping => m.column +: Stream()
      }
  lazy val columns = compositeColumns

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

  lazy val deepContainedMappings : Stream[Mapping]
    = mappings flatMap {
        case m : CompositeMapping => m +: m.deepContainedMappings
        case m => m +: Stream()
      }

}
