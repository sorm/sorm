package sorm.core

import sorm.core.util._
import sorm.core.api._
import Setup._

trait API[members <: Product] {

  import language.experimental.macros

  type Persisted = api.Persisted
  type PersistedMixiner[a] = api.PersistedMixiner[a]
  type Driver

  def insert[e](e: e)(implicit member: Member[e]): e with Persisted =
    ???

  def update[e: Member](e: e) = ???


}
object API {

}