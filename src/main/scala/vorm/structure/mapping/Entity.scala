package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Entity
  ( val settings : Settings,
    val reflection : Reflection,
    val parent : Mapping )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = reflection.properties
          .map { case (name, r) ⇒ new EntityProperty( settings, r, this, name ) }
          .toList
  }
