package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Map
  ( val reflection : Reflection,
    val parent : Mapping,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = new MapKey( reflection.generics(0), this, settings ) ::
        new MapValue( reflection.generics(1), this, settings ) ::
        Nil
  }
