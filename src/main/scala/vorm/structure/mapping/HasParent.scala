package vorm.structure.mapping

import vorm._
import structure._
import reflection._

trait HasParent extends Mapping with HasReflection {
  def parent : Mapping
  lazy val parentTableMapping
    : scala.Option[Table]
    = parent match {
          case parent : Table
            ⇒ Some(parent)
          case parent : Mapping
            ⇒ parent.parentTableMapping
        }

}

