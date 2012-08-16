package vorm.selectComposition

import vorm._
import persisted._
import structure._
import query._
import mapping._
import sql._
import ddl._
import extensions._

object Composition {

  def intersect
    ( left : Select,
      right : Select )
    : Executable
    = left narrow right

  /**
   * A query uniting the results of inputs
   */
  def union
    ( left : Executable,
      right : Executable )
    : Executable
    = Union( left, right )

  def countItems
    ( m : CollectionMapping )
    : Select
    = ???


}