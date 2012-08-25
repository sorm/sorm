package sorm.reflection

import reflect.runtime.universe._

trait Api {
  def t : Type

//  def isMixedIn = t.kind == "RefinedType"
//
//  /**
//   * Either the type itself if it's not mixed in or the first of its parents
//   */
//  def mixinBasis = if(isMixedIn) t.baseClasses.head else t

  def members : Stream[Symbol]
    = t.members.toStream

  def properties = members.filter{_.isTerm}.filter{!_.isMethod}

}
