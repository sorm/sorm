package sorm.core

import sorm.core.util._
import sorm.core.api._

trait API[members <: Product] {

  import language.experimental.macros

  type Persisted = api.Persisted
  type PersistedMixiner[a] = api.PersistedMixiner[a]
  type Driver

}
object API {

}