package sorm

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

trait Api extends Logging {

  protected val connector : Connector

  protected def mapping[ T : TypeTag ] : EntityMapping

  /**
   * Return the [[sorm.Access]] object for performing a read-query on a specified entity type.
   * 
   * @tparam T The entity type
   * @return The accessor object. An abstraction over all kinds of supported SELECT-queries.
   */
  def access [ T <: AnyRef : TypeTag ] 
    = Access[T](mapping, connector)

  /**
   * Fetch an existing entity by id. Will throw an exception if the entity doesn't exist. 
   * @param id The id
   * @return An entity instance with a [[sorm.Persisted]] trait mixed in
   */
  def fetchById
    [ T <: AnyRef : TypeTag ]
    ( id : Long )
    : T with Persisted
    = connector.withConnection{ cx => 
        id $ ("id" -> _) $ (Map(_)) $ (mapping[T].fetchByPrimaryKey(_, cx).asInstanceOf[T with Persisted])
      }

  /**
   * Save the entity. An Abstraction over INSERT and UPDATE-queries. Which one to perform will be decided based on whether the [[sorm.Persisted]] trait is mixed in the value you provide.
   * @param value The value to save
   * @return The saved entity instance with a [[sorm.Persisted]] trait mixed in
   */
  def save
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = connector.withConnection{ cx =>
        mapping[T].save(value, cx).asInstanceOf[T with Persisted]
      }

  /**
   * Saves the entity by overwriting the existing one if one with the matching unique keys exists and creating a new one otherwise. Executing simply [[sorm.Api#save]] in a situation of unique keys clash would have thrown an exception. Beware that in case when not all unique keys are matched this method will still throw an exception.
   * @param value The value to save
   * @return The saved entity instance with a [[sorm.Persisted]] trait mixed in
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
          connector.withConnection{ cx =>
            cx.transaction {
              access.fetchOneId()
                .map(Persisted(value, _))
                .getOrElse(value)
                .$(mapping[T].save(_, cx).asInstanceOf[T with Persisted])
            }
          }
        )

  def delete
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    = connector.withConnection{ cx => mapping[T].delete(value, cx) }

  /**
   * Perform several db-requests in a single transaction. For most dbs this provides guarantees that nothing will be changed in between the db-requests in multithreaded applications and that it will roll-back in case of any failure.
   *
   * All db-requests which should be executed as part of this transaction must be run on the same thread this method gets called on.
   *
   * Use transactions with care because for the time the transaction is being executed the involved tables are supposed to get locked, putting all the requests to them from other threads in a queue until the current transaction finishes. The best practice is to make transactions as short as possible and to perform any calculations prior to entering transaction.
   * 
   * @param t The closure wrapping the actions performed in a single transaction.
   * @tparam T The result of the closure
   * @return The result of the last statement of the passed in closure
   */
  def transaction [ T ] ( t : => T ) : T
    = connector.withConnection{ cx => cx.transaction(t) }

  /**
   * Current time at DB server in milliseconds. Effectively fetches the date only once to calculate the deviation.
   */
  lazy val nowMillis = connector.withConnection { cx =>
    val deviation = System.currentTimeMillis() - cx.now().getMillis
    () => System.currentTimeMillis() - deviation
  }

  /**
   * Current DateTime at DB server. Effectively fetches the date only once to calculate the deviation.
   */
  def now() = new DateTime(nowMillis())

}
