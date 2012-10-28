package sorm.mappings

import sext._, embrace._

import sorm._
import reflection.Reflection
import core._
import ddl._

trait CompositeMapping extends Mapping {

  def mappings : Stream[Mapping]

  lazy val compositeColumns : Stream[Column]
    = mappings flatMap (_.columnsForContainer)
  def columnsForContainer : Stream[Column] = compositeColumns

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
