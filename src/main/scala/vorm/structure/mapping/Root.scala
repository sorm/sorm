package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Root
  ( val settings : Map[Reflection, EntitySettings] )
  extends Mapping with HasChildren
  {
    lazy val children
      = settings.keys.map( Mapping( settings, _, this) ).toList
  }
