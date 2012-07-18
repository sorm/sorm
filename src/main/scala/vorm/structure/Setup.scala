package vorm.structure

import vorm._
import reflection._

trait Setup {

  def primaryKey
  : Reflection => Option[Seq[String]]

  def uniqueKeys
  : Reflection => Set[Seq[String]]

  def indexes
  : Reflection => Set[Seq[String]]

  def autoIncremented
  : (Reflection, String) => Boolean

}