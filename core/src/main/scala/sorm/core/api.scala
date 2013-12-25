package sorm.core

trait API {

  import language.experimental.macros

  type Persisted = api.Persisted
  type PersistedMixiner[a] = api.PersistedMixiner[a]
  type Driver

  private object settingsBuilder {

    import api.Settings._
    import reflect.runtime.universe._
    import collection.{mutable => M}

    private val map = M.Map[Type, M.Set[Key]]()
    def addEntityType(t: Type) {
      map.getOrElseUpdate(t, M.Set())
    }
    def addKey(t: Type, k: Key) {
      map.getOrElseUpdate(t, M.Set()).add(k)
    }
    def freeze: Settings = map.view.map(kv => (kv._1, kv._2.toSet)).toMap
  }

  /**
   * Register an entity type and derive a `PersistedMixiner` typeclass instance.
   * @tparam a The type of the entity
   */
  protected def entity[a]: Unit = ???

  protected def uniqueKey[entity](refs: (entity => _)*): Unit = ???

  def save[e: PersistedMixiner](e: e): e with Persisted

}
object API {

}