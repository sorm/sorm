package sorm.structure.mapping

import sorm._
import extensions._
import reflection._
import ddl._
import structure._

trait HasChildren
  extends Mapping
  {
    def children : Iterable[Mapping]

    lazy val leaves : Set[Mapping]
      = children
          .view
          .flatMap{
            case c : HasChildren ⇒ c.leaves
            case c ⇒ c :: Nil
          }
          .toSet

    lazy val nestedValueMappings : Set[ValueMapping]
      = leaves.asInstanceOf[Set[ValueMapping]]
  }