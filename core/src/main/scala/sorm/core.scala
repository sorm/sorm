package sorm; package object core {



/**
 * A list of references from a single entity, representing an abstraction over
 * a database index.
 *
 * TODO: to provide a concrete implementation.
 */
sealed trait Key[ entity, fields ] {
  /**
   *
   */
  val symbols : Seq[ Symbol ]
  // TODO: implement hashCode and eq
}
case class UniqueKey[e, v](symbols : Seq[ Symbol ]) extends Key[e, v]
case class NonUnique[e, v](symbols : Seq[ Symbol ]) extends Key[e, v]

trait Persisted {
  val id : Long
}

/**
 * The target for macro-conversion. The plan is that instances of this type
 * become constructors of the parameter type mixed in with Persisted by means of
 * the generated `mixinPersisted` method by a macro.
 */
trait Entity[ a ] {
  val keys : Set[ Key[ a, Any ] ]
  def mixinPersisted( value : a, id : Long ) : a with Persisted
}

/**
 * Exports of this module to be mixed into the public API, e.g. `sorm._`.
 */
trait Exports {
  import language.experimental.macros

  type Persisted = sorm.core.Persisted
  type Entity[a] = sorm.core.Entity[a]

  /**
   * The macro conversion should just proxy the `indexed` and `unique`
   * parameters.
   */
  def entity[ a ]
    ( keys : Set[ Key[ a, Any ] ] )
    : Entity[ a ] 
    = macro Macros.entity[ a ]

  def uniqueKey[ entity, fields ]( f : entity => fields )
    : UniqueKey[ entity, fields ]
    = macro Macros.uniqueKey[ entity, fields ]

}

}
