package vorm.api

import vorm._
import reflection._
import structure._

sealed case class Entity
  [ T : TypeTag ]
  ( uniqueKeys    : Set[Seq[String]] = Set(),
    indexes       : Set[Seq[String]] = Set() )
  {
    // here should be tests on validity of provided data
    def reflection
      = Reflection[T]
    def settings
      = EntitySettings(uniqueKeys, indexes)
  }