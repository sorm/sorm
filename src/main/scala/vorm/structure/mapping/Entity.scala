package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Entity
  ( val reflection : Reflection,
    val parent : Mapping,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = reflection.properties
          .map { case (n, r) ⇒ new EntityProperty( n, r, this, settings ) }
          .toList

    lazy val settings1 = settings(reflection)
  }
