package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Seq
  ( val settings : Settings,
    val reflection : Reflection,
    val parent : Mapping )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = new SeqItem( settings, reflection.generics(0), this ) ::
        Nil
  }
