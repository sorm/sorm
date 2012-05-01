package vorm.api

import org.joda.time.DateTime

trait API {
  /**
   * Current time at DB server
   */
  def date: DateTime

  /**
   * Replaces insertUpdateDelete approach because it didn't allow to execute inserts
   * prior to updates to be able to manipulate their ids.
   */
  def transaction(f: () => Unit)

  def save[T](value: T): T with Persisted

  def save[T](values: List[T]): List[T with Persisted]

  def all[T]: QueryStream[T with Persisted]

}
