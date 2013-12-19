package sorm; package object core {

import language.experimental.macros
import reflect.runtime.universe._
import reflect.macros.Context


sealed trait Persisted {
  val id : Long
}

sealed trait Key[ entity, fields ] {
  /**
   *
   */
  val symbols : Seq[ Symbol ]
  // TODO: implement hashCode and eq
}
trait UniqueKey[e, v] extends Key[e, v]
trait NonUnique[e, v] extends Key[e, v]

sealed trait Entity[ a ] {
  def toPersisted( value : a, id : Long ) : a with Persisted
  // TODO: implement hashCode and eq
}

/**
 * Exports of this module into the public API, e.g. `sorm._`.
 */
trait Exports {

  type Persisted = sorm.core.Persisted

  def entity[ a ]
    ( keys : Set[ Key[ a, _ ] ] )
    : Entity[ a ]
    = macro Macros.entity[ a ]

  def uniqueKey[ entity, fields ]( f : entity => fields )
    : UniqueKey[ entity, fields ]
    = macro Macros.uniqueKey[ entity, fields ]

}

}

