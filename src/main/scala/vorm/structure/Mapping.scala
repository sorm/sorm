package vorm.structure

import vorm.reflection._

trait Mapping {
  def membership : Option[Membership[Mapping]]
}
object Mapping {
  
  def apply 
    ( reflection : Reflection,
      setup : Setup )
    : Mapping
    = apply( None, reflection, setup )

  def apply 
    ( membership : Membership[Mapping],
      reflection : Reflection,
      setup : Setup )
    : Mapping
    = apply( Some(membership), reflection, setup )

  def apply 
    ( membership : Option[Membership[Mapping]],
      reflection : Reflection,
      setup : Setup )
    : Mapping
    = throw new NotImplementedError

}