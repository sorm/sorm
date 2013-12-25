package sorm.core.api


/**
 * A meta-info on DB structure for use in runtime.
 */
object Settings {
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

}
