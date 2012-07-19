package vorm.structure

import vorm._
import structure._
import reflection._

package object mapping {

  trait HasParent {
    def parent : Mapping
  }

  trait HasChildren {
    def children : Seq[Mapping]
  }

  trait HasReflection {
    def reflection : Reflection
  }

}