package vorm.structure.mapping

import vorm._
import structure._
import reflection._

trait Mapping {
  def settings : Map[Reflection, EntitySettings]
}
object Mapping {
  /**
   * Root
   */
  def apply
    ( settings : Map[Reflection, EntitySettings] )
    = new Root( settings )
    
  def apply
    ( settings : Map[Reflection, EntitySettings],
      reflection : Reflection,
      parent : Mapping )
    : Mapping
    = throw new NotImplementedError

}