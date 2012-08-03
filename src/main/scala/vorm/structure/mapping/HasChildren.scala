package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

trait HasChildren
  extends Mapping
  {
    def children : Iterable[Mapping]

    lazy val leaves
      : Iterable[Mapping]
      = children.flatMap {
          case c : HasChildren ⇒ c.leaves
          case c ⇒ c :: Nil
        }

    lazy val nestedValueMappings
      = leaves.asInstanceOf[Iterable[ValueMapping]]
  }