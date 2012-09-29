package sorm.newMappings

import sorm._
import ddl._

trait ColumnMapping extends Mapping {
  def column : Column

}