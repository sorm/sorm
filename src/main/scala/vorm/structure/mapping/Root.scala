package vorm.structure.mapping

import vorm._
import structure._
import reflection._

class Root
  ( val settings : Settings )
  extends Mapping with HasChildren
  {
    lazy val children
      = settings.keys.map( new RootChild( _, this, settings ) ).toList
  }
