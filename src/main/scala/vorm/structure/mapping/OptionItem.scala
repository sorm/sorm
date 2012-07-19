package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class OptionItem
  ( val reflection : Reflection,
    val parent : Option,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChild
  with HasReflection
  {
    lazy val child
      = Mapping( reflection, this, settings )
  }
