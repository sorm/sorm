package sorm.core

import language.experimental.macros
import reflect.runtime.universe._
import reflect.macros.Context


/**
 * A list of references from a single entity, representing an abstraction over
 * a database index.
 *
 * TODO: to provide a concrete implementation.
 */
trait Key[ EntityT ]

trait Persisted {
  val id : Long
}

/**
 * The target for macro-conversion. The plan is that instances of this type
 * become constructors of the parameter type mixed in with Persisted by means of
 * the generated `mixinPersisted` method by a macro.
 */
trait Entity[ T ] {
  val indexed : Set[ Key[ T ] ]
  val unique : Set[ Key[ T ] ]
  def mixinPersisted( value : T, idValue : Long ) : T with Persisted 
}


/**
 * A container object for all module's macros
 */
private object Macros {

  def entity
    [ T : c.WeakTypeTag ]
    ( c : Context )
    ( indexed : c.Expr[ Set[ Key[ T ] ] ], 
      unique : c.Expr[ Set[ Key[ T ] ] ] )
    : c.Expr[ Entity[ T ] ]
    = ???

}


/**
 * Exports of this module to be mixed into the public API, e.g. `sorm._`.
 */
trait Exports {

  type Persisted = sorm.core.Persisted

  /**
   * The macro conversion should just proxy the `indexed` and `unique`
   * parameters.
   */
  def entity[ T ]
    ( indexed : Set[ Key[ T ] ],
      unique : Set[ Key[ T ] ] )
    : Entity[ T ] 
    = macro Macros.entity[ T ]

}

