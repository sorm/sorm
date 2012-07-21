package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Option
  ( val reflection : Reflection,
    val parent : Mapping,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChild
  with HasReflection
  {
    lazy val child
      = new OptionItem( reflection.generics(0), this, settings )
  }
