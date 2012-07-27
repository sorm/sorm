package vorm.structure

import vorm._
import structure._
import reflection._
import mapping._

trait Mapping {
  def settings : Settings
  lazy val parentTableMapping
    : scala.Option[Table]
    = ???
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