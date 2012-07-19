package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class SeqItem
  ( val reflection : Reflection,
    val parent : Mapping,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = Mapping( reflection, this, settings ) ::
        Nil
  }