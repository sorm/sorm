package sorm.relational

import sorm.core._
import sorm.relational._
import reflect.runtime.{universe => ru}

/**
 * Rules for dynamic resolution of inter-type relations.
 */
object rules {

  def columnName( t: ru.Type ): String = ???

  def bindingsToParent( rep: mapping.Rep ): Seq[(String, String)] = rep match {
    case mapping.Rep.CaseClass(properties) => ???
    case _ => ???
  }
  
}
