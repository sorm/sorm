package sorm

import sext._

import sorm._
import reflection.Reflection
import core._

package object mappings {
  def ddlName ( string : String )
    : String
    = {
      import com.google.common.base.CaseFormat._
      UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
    }


}