package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class EntityProperty
  ( val settings : Settings,
    val reflection : Reflection,
    val parent : Mapping,
    val name : String )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = Mapping( settings, reflection, this ) ::
        Nil
  }
