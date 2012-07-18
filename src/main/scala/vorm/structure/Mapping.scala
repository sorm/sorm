package vorm.structure

import vorm.reflection._

trait Mapping {
  def membership : Option[Membership]
}
object Mapping {
  
  def apply 
    ( reflection : Reflection,
      setup : Setup )
    : Mapping
    = apply( None, reflection, setup )

  def apply 
    ( membership : Membership,
      reflection : Reflection,
      setup : Setup )
    : Mapping
    = apply( Some(membership), reflection, setup )

  def apply 
    ( membership : Option[Membership],
      reflection : Reflection,
      setup : Setup )
    : Mapping
    = throw new NotImplementedError

}