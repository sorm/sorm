package vorm.structure

import vorm._
import structure._
import reflection._

package object mapping {

  trait HasParent {
    def parent : Mapping
  }

  trait HasChildren {
    def children : collection.Seq[Mapping]
  }

  trait HasChild {
    def child : Mapping
  }

  trait HasReflection {
    def reflection : Reflection
  }

  type Settings = collection.Map[Reflection, EntitySettings]

}