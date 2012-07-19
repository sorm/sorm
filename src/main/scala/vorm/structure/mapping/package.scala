package vorm.structure

import vorm._
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

  // case class Root
  //   ( val setup : Setup )
  //   extends Mapping with HasChildren
  //   {
  //     lazy val children
  //       = setup.entities.map( Mapping( setup, this) )
  //   }

}