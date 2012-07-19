package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class SeqItem
  ( val settings : Settings,
    val reflection : Reflection,
    val parent : Seq )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = Mapping( settings, reflection, this ) ::
        Nil
  }