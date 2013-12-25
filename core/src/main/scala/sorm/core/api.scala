package sorm.core

import sorm.core.util._
import sorm.core.api._

trait API[members <: HList] extends Setup[members] {

  import language.experimental.macros

  type Persisted = api.Persisted
  type PersistedMixiner[a] = api.PersistedMixiner[a]
  type Driver

  /**
   * Register an entity type and derive a `PersistedMixiner` typeclass instance.
   * @tparam a The type of the entity
   */
  protected def entity[a]: Unit = ???

  protected def uniqueKey[entity](refs: (entity => _)*): Unit = ???

  def save[e: Member](e: e): e with Persisted


}
object API {

}