package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Tuple
  ( val setup : Setup,
    val reflection : Reflection,
    val parent : Mapping )
  extends Mapping
  with HasParent
  with HasChildren
  with HasReflection
  {
    lazy val children
      = reflection.generics.view
          .zipWithIndex.map { case (r, i) ⇒ new TupleItem(setup, r, this) }
          .toList

  }

