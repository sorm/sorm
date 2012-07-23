package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Tuple
  ( val reflection : Reflection,
    val parent : Mapping,
    val settings : Settings )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = reflection.generics
          .view
          .zipWithIndex
          .map { case (r, i) ⇒ new TupleItem(i, r, this, settings) }
          .toList

  }

