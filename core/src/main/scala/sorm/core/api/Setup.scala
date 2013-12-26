package sorm.core.api

import shapeless._

class Setup[tuple <: Product, hlist <: HList](tuple: tuple)(implicit tupleGeneric: Generic.Aux[tuple, hlist]) {

  import Setup._

  private val hlist: hlist = tupleGeneric.to(tuple)
  implicit def memberInstance[a](implicit selector: ops.hlist.Selector[hlist, Member[a]]): Member[a] =
    selector.apply(hlist)

}
/**
 * A meta-info on DB structure for use in runtime.
 */
object Setup {

  import reflect.runtime.universe._

  /**
   * Associations of entity types and keys.
   */
  type Settings = Map[Type, Set[Key]]

  case class Key(flavour: KeyFlavour, fields: Seq[Symbol])

  sealed trait KeyFlavour
  object KeyFlavour {
    case object Unique extends KeyFlavour
    case object NonUnique extends KeyFlavour
  }

  @annotation.implicitNotFound(msg = "A Member[${a}] instance is not declared")
  class Member[a](persistedMixiner: PersistedMixiner[a], keys: Set[Key])

}
