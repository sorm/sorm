package vorm.mapping

import vorm._
import reflection._

trait Setup {
  def primaryKey      : Map[Reflection, Option[Seq[String]]]
  def uniqueKeys      : Map[Reflection, Set[Seq[String]]]
  def indexes         : Map[Reflection, Set[Seq[String]]]
  def autoIncrements  : Map[Reflection, Set[String]]
}