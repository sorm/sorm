package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Value
  ( val settings : Map[Reflection, EntitySettings],
    val reflection : Reflection,
    val parent : Mapping )
  extends Mapping
  with HasParent
  with HasReflection
  {
  }
