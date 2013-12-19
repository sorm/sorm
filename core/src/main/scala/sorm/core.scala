package sorm; package object core {



/**
 * A list of references from a single entity, representing an abstraction over
 * a database index.
 *
 * TODO: to provide a concrete implementation.
 */
trait Key[ entity ]

trait Persisted {
  val id : Long
}

/**
 * The target for macro-conversion. The plan is that instances of this type
 * become constructors of the parameter type mixed in with Persisted by means of
 * the generated `mixinPersisted` method by a macro.
 */
trait Entity[ t ] {
  val indexed : Set[ Key[ t ] ]
  val unique : Set[ Key[ t ] ]
  def mixinPersisted( value : t, idValue : Long ) : t with Persisted 
}

/**
 * Exports of this module to be mixed into the public API, e.g. `sorm._`.
 */
trait Exports {
  import language.experimental.macros

  type Persisted = sorm.core.Persisted
  type Entity[T] = sorm.core.Entity[T]

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

}