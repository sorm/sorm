package sorm.relational

import sorm.core._
import reflect.runtime.{universe => ru}

/**
 * Rules for dynamic resolution of inter-type relations.
 */
object rules {

  def columnName( t: ru.Type ): String = ???
  def bindings( from: ru.Type, to: ru.Type ): Seq[(String, String)] = 
    ???

  //  ---------- OR -------------
  // def bindingsTo
  // def bindingsFrom
  
}
