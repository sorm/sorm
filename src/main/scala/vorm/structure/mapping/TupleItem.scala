package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class TupleItem
  ( val settings : Settings,
    val reflection : Reflection,
    val parent : Mapping,
    val index : Int )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = Mapping( settings, reflection, this ) ::
        Nil
  }
