package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class SeqItem
  ( val reflection : Reflection,
    val parent : Seq,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChild
  with HasReflection
  {
    lazy val child
      = Mapping( reflection, this, settings )
  }