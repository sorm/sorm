package sorm.core

import reflect.basis._
import sorm._
import persisted._
import query.AbstractSqlComposition
import reflection._
import mappings._
import jdbc._
import query.Query._
import sext._

import com.weiglewilczek.slf4s.Logging

trait Api extends Logging with CurrentDateTime {

  protected def driver : Driver

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

  def access [ T <: AnyRef : TypeTag ] 
    = Access[T](mapping, driver)

  def fetchById
    [ T <: AnyRef : TypeTag ]
    ( id : Long )
    : T with Persisted
    = id $ ("id" -> _) $ (Map(_)) $ (mapping[T].fetchByPrimaryKey(_).asInstanceOf[T with Persisted])

  def save
    [ T <: AnyRef : TypeTag ]
    ( value : T )
    : T with Persisted
    = transaction {
        mapping[T].save(value).asInstanceOf[T with Persisted]
      }

  /**
   * Safely saves
   * @param v
   * @tparam T
   * @return
   */
  def saveByUniqueKeys
    [ T <: AnyRef : TypeTag ]
    ( v : T )
    : T with Persisted
    = (mapping[T].uniqueKeys.flatten zipBy v.reflected.propertyValue)
        //  todo: check the unique entities
        .ensuring(_.nonEmpty, "Type doesn't have unique keys")
        .foldLeft(access){ case (access, (n, v)) => access.whereEqual(n, v) }
        .$(access =>
          transaction {
            access.fetchOneId()
              .map(Persisted(v, _))
              .getOrElse(v)
              .$(mapping[T].save(_).asInstanceOf[T with Persisted])
          }
        )


  def transaction [ T ] ( t : => T ) : T = driver.transaction(t)
  def transaction [ T ] ( t : Api => T ) : T = driver.transaction(t(this))

}
