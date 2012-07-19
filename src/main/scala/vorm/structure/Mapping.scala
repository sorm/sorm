package vorm.structure

import vorm._
import structure._
import reflection._
import mapping._

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
    ( reflection : Reflection,
      parent : Mapping,
      settings : Settings )
    : Mapping
    = throw new NotImplementedError

}