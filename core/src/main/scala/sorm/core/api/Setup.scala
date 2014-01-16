package sorm.core.api

import shapeless._

object Setup {

  /**
   * A meta-info on DB structure for use in runtime.
   */
  class Members[tuple <: Product, hlist <: HList](tuple: tuple)(implicit tupleGeneric: Generic.Aux[tuple, hlist]) {
    private val hlist: hlist = tupleGeneric.to(tuple)
    implicit def memberInstance[a](implicit selector: ops.hlist.Selector[hlist, Member[a]]): Member[a] =
      selector.apply(hlist)
  }

  type Key = Seq[Symbol]

  // NOTE: Not a case class, so that it isn't a Product
  @annotation.implicitNotFound(msg = "A Member[${a}] instance is not declared")
  class Member[a](persistedMixiner: PersistedMixiner[a], uniqueKeys: Set[Key], nonUniqueKeys: Set[Key])

  def key[e](refs: (e => _)*): Key = ???
  def member[e](uniqueKeys: Set[Key], nonUniqueKeys: Set[Key]): Member[e] = ???

}
