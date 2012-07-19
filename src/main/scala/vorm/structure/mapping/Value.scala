package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Value
  ( val reflection : Reflection,
    val parent : Mapping,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasReflection
  {
  }
