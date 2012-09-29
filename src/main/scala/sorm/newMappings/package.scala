package sorm

import sext.Sext._

import sorm._
import reflection.Reflection
import core._

package object newMappings {
  def ddlName ( string : String )
    : String
    = {
      import com.google.common.base.CaseFormat._
      UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
    }


  
  // trait MasterTableMapping extends TableMapping {
  //   def name = ddlName(reflection.name)
  // }
  // trait SlaveTableMapping extends TableMapping {
  //   def name = ???
  // }

}