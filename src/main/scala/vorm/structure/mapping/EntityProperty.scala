package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class EntityProperty
  ( val name : String,
    val reflection : Reflection,
    val parent : Entity,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChild
  with HasReflection
  {
    lazy val child
      = Mapping( reflection, this, settings )
  }
