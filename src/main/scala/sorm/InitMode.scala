package sorm

/**
 * The mode for initialization performed when a connection to the db is
 * established on creation of SORM instance.
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
   * Just generate the tables. Fail if name conflicts arise with the existing
   * ones
   */
  case object Create extends InitMode
  /**
   * Do nothing
   */
  case object DoNothing extends InitMode
}