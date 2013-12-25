package sorm.core.api

import sorm.core.util.HList

abstract class Setup[members](members: members) {

  type Member[a] = Setup.Member[a]

  implicit def memberInstance[a](implicit elementInstance: HList.Element[members, Setup.Member[a]]) =
    elementInstance.get(members)

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
  case class Member[a](persistedMixiner: PersistedMixiner[a], keys: Set[Key])

}
