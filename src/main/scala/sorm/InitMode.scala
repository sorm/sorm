package sorm

/**
 * A mode for initialization performed when a connection to db gets established
 * on creation of a SORM instance.
 */
sealed trait InitMode
object InitMode {
  /**
   * Wipe out all the contents of the db and generate the tables
   */
  case object DropAllCreate extends InitMode
  /**
   * Drop only the tables which have conflicting names with the ones to be
   * generated and actually generate them
   */
  case object DropCreate extends InitMode
  /**
   * Try to generate the tables if they don’t already exist.
   */
  case object Create extends InitMode
  /**
   * Do nothing
   */
  case object DoNothing extends InitMode
}