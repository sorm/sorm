package vorm.structure.mapping

import vorm.reflection._
import vorm.structure.Setup

trait Mapping {
  def setup: Setup
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

}