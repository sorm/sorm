package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Seq
  ( val reflection : Reflection,
    val parent : Mapping,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = new SeqItem( reflection.generics(0), this, settings ) ::
        Nil
  }
