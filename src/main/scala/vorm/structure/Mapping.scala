package vorm.structure

import vorm.reflection._

trait Mapping {
  def setup : Setup
}
object Mapping {
  /**
   * Root
   */
  def apply 
    ( setup : Setup,
      reflection : Reflection )
    : Mapping
    = throw new NotImplementedError

  def apply 
    ( setup : Setup,
      reflection : Reflection,
      parent : Mapping )
    : Mapping
    = throw new NotImplementedError

  trait HasParent {
    def parent : Mapping
  }

  trait HasChildren {
    def children : Seq[Mapping]
  }

  trait HasReflection {
    def reflection : Reflection
  }

  case class Tuple
    ( setup : Setup,
      reflection : Reflection,
      parent : Mapping )
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

  case class TupleItem
    ( setup : Setup,
      reflection : Reflection,
      parent : Mapping )
    extends Mapping 
    with HasParent 
    with HasChildren
    with HasReflection
    {
      lazy val children
        = Mapping( setup, reflection, this ) ::
          Nil
    }



  // case class Root
  //   ( setup : Setup )
  //   extends Mapping with HasChildren
  //   {
  //     lazy val children
  //       = setup.entities.map( Mapping( setup, this) )
  //   }

}