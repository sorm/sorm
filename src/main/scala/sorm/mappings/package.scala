package sorm.mappings

import sext._

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


}