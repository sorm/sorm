package sorm

import sorm._
import reflection._

import reflect.runtime.universe._

sealed case class Entity
  ( reflection  : Reflection,
    indexed     : Set[Seq[String]],
    unique      : Set[Seq[String]] )
object Entity {
  /**
   * Entity settings. Used for registering entities with the SORM instance.
   * @param indexed
   * Fields of case class on which the filtering operations will be performed
   * when querying the db. Specifying those may help the DB to perform a little
   * better.
   * @param unique
   * Fields of case class which are known to have a unique value amongst all
   * possible instances of this class. Specifying these may help the db perform
   * a little better and will protect you from storing entities having duplicate
   * values of this field.
   * @tparam T The case class
   */
  def apply
    [ T : TypeTag ]
    ( indexed : Set[Seq[String]] = Set(),
      unique : Set[Seq[String]] = Set() )
    : Entity
    = Entity(Reflection[T], indexed, unique)
}

