package sorm.core.api

import sorm.core._

/**
 * A list of references from a single entity, representing an abstraction over
 * a database index.
 */
// TODO: if we go with typeclasses for entities, why not for this too?
sealed trait Key[ entity, fields ] {
  val fields: Seq[FieldRef[entity, _]]
}
object Key {
  case class Unique[e, v](fields: Seq[FieldRef[e, _]]) extends Key[e, v]
  case class NonUnique[e, v](fields: Seq[FieldRef[e, _]]) extends Key[e, v]
}

trait Persisted {
  val id : Long
}

/**
 * A type-class holding settings of how an entity type should be treated and
 * a `mixinPersisted` method, which generates `Persisted` instances.
 */
// NOTE: possible problem: for analyzing entities,
// we'll require a facility to traverse all visible instances of this type.
@annotation.implicitNotFound(msg = "Entity of type ${a} is not declared")
trait Entity[ a ] {
  val keys : Set[ Key[ a, Any ] ]
  def mixinPersisted( value : a, id : Long ) : a with Persisted
}

trait DeclaresSettings {

  import language.experimental.macros

  type Persisted = api.Persisted
  type Entity[a] = api.Entity[a]
  type Key[entity, fields] = api.Key[entity, fields]
  val Key = api.Key

  def entity[ a ]
    ( keys : Set[ Key[ a, Any ] ] )
    : Entity[ a ]
    = macro api.Macros.entity[ a ]

  def uniqueKey[ entity, fields ]( f : entity => fields )
    : Key.Unique[ entity, fields ]
    = macro api.Macros.uniqueKey[ entity, fields ]

}
