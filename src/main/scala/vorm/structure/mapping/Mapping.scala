package vorm.structure.mapping

import vorm._
import structure._
import reflection._

trait Mapping {
  def settings : Settings
}
object Mapping {
  /**
   * Root
   */
  def apply
    ( settings : Settings )
    = new Root( settings )
    
  def apply
    ( settings : Settings,
      reflection : Reflection,
      parent : Mapping )
    : Mapping
    = throw new NotImplementedError

}