package sorm.mappings

import sext._, embrace._

import sorm._
import reflection.Reflection
import core._

object `package` {
  def ddlName ( string : String )
    : String
    = {
      import com.google.common.base.CaseFormat._
      UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
    }

  case class EntitySettings
    ( indexes       : Set[Seq[String]] = Set.empty,
      uniqueKeys    : Set[Seq[String]] = Set.empty )

}