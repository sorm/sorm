package sorm.api

import reflect.runtime.universe._
import sorm._
import driver.DriverConnection
import core._
import persisted._
import reflection._
import mappings._
import sext._, embrace._

import com.weiglewilczek.slf4s.Logging
import org.joda.time.DateTime

trait Connection extends Logging {

  protected def connection : DriverConnection

  protected def mappings : Map[Reflection, EntityMapping]

  private def mapping
    [ T : TypeTag ]
    = {
      def mapping( r : Reflection ) 
        = mappings.get(r)
            .getOrElse {
              throw new SormException(
                "Entity `" + r.name + "` is not registered"
              )
            }
      mapping(Reflection[T].mixinBasis)
    }

  /**
   * Return the [[sorm.api.Access]] object for performing a read-query on a specified entity type.
   * 
   * @tparam T The entity type
   * @return The accessor object. An abstraction over all kinds of supported SELECT-queries.
   */
  def access [ T <: AnyRef : TypeTag ] 
    = Access[T](mapping, connection)

  /**
   * Fetch an existing entity by id. Will throw an exception if the entity doesn't exist. 
   * @param id The id
   * @return An entity instance with a [[sorm.persisted.Persisted]] trait mixed in
   */
  def fetchById
    [ T <: AnyRef : TypeTag ]
    ( id : Long )
    : T with Persisted
    = id $ ("id" -> _) $ (Map(_)) $ (mapping[T].fetchByPrimaryKey(_, connection).asInstanceOf[T with Persisted])

  /**
   * Save the entity. An Abstraction over INSERT and UPDATE-queries. Which one to perform will be decided based on whether the [[sorm.persisted.Persisted]] trait is mixed in the value you provide.
   * @param value The value to save
   * @return The saved entity instance with a [[sorm.persisted.Persisted]] trait mixed in
   */
  def save
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = transaction {
        mapping[T].save(value, connection).asInstanceOf[T with Persisted]
      }

  /**
   * Saves the entity by overwriting the existing one if one with the matching unique keys exists and creating a new one otherwise. Executing simply [[sorm.api.Connection#save]] in situation of unique keys clash would have thrown an exception. Please beware that in case when not all unique keys are matched this method will still throw an exception.
   * @param value The value to save
   * @return The saved entity instance with a [[sorm.persisted.Persisted]] trait mixed in
   */
  def saveByUniqueKeys
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = (mapping[T].uniqueKeys.flatten zipBy value.reflected.propertyValue)
        //  todo: check the unique entities
        .ensuring(_.nonEmpty, "Type doesn't have unique keys")
        .foldLeft(access){ case (access, (n, v)) => access.whereEqual(n, v) }
        .$(access =>
          transaction {
            access.fetchOneId()
              .map(Persisted(value, _))
              .getOrElse(value)
              .$(mapping[T].save(_, connection).asInstanceOf[T with Persisted])
          }
        )

  def delete
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    = mapping[T].delete(value, connection)

  /**
   * Perform several db-requests in a single transaction. This provides guarantees that nothing will be changed in between the db-requests in multi-treaded applications and that it will roll-back in case of any failure.
   *
   * Use it with care because for the time the transaction is being executed the involved tables get locked putting all the requests to them from other threads in a queue until the current transaction finishes. Best practice is to make transactions as short as possible and to perform any calculations prior to entering transaction.
   * 
   * @param t The closure wrapping the actions performed in a single transaction.
   * @tparam T The result of the closure
   * @return The result of the last statement of the passed in closure
   */
  def transaction [ T ] ( t : => T ) : T = connection.transaction(t)

  /**
   * Same as the other version with an exception that it passes the current SORM instance as a parameter to the closure. 
   * @param t The SORM instance
   * @tparam T The result of the closure
   * @return The result of the last statement of the passed in closure
   */
  def transaction [ T ] ( t : Connection => T ) : T = connection.transaction(t(this))

  /**
   * Current time at DB server in milliseconds. Effectively fetches the date only once to calculate the deviation.
   */
  lazy val nowMillis = {
    val deviation = System.currentTimeMillis() - connection.now().getMillis
    () => System.currentTimeMillis() - deviation
  }

  /**
   * Current DateTime at DB server. Effectively fetches the date only once to calculate the deviation.
   */
  def now() = new DateTime(nowMillis())

  /**
   * Close the underlying jdbc connection. Call it when in a running application this connection will not be used anymore. For example when the actor that was using this connection is killed.
   */
  def close() = connection.close()

}
