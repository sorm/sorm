package vorm.structure

import vorm._
import reflection._

trait Setup {

  def primaryKey 
    ( reflection : Reflection )
    : Option[Seq[String]]

  def uniqueKeys
    ( reflection : Reflection )
    : Set[Seq[String]]

  def indexes
    ( reflection : Reflection )
    : Set[Seq[String]]

  def isAutoIncremented
    ( reflection : Reflection, 
      property : String )
    : Boolean

}