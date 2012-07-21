package vorm.structure.mapping

import vorm._
import structure._
import reflection._

trait HasChildren {
  def children : collection.Seq[Mapping]
}
