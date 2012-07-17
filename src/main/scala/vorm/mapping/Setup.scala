package vorm.mapping

import vorm._
import reflection._

trait Setup {
  
  def primaryKeys
    : Map[Reflection, Seq[String]]

  def uniqueKeys
    : Map[Reflection, Set[Seq[String]]]

  def indexes
    : Map[Reflection, Set[Seq[String]]]

  def autoIncrements
    : Set[(Reflection, String)]

}