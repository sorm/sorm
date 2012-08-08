package vorm.api

import vorm._
import reflection._
import structure._

sealed case class Entity
  [ T : TypeTag ]
  ( primaryKey    : Seq[String] = Seq(),
    uniqueKeys    : Set[Seq[String]] = Set(),
    indexes       : Set[Seq[String]] = Set(),
    autoIncrement : Set[String] = Set() )
  {
    // here should be tests on validity of provided data
    def reflection
      = Reflection[T]
    def settings
      = EntitySettings(primaryKey, uniqueKeys, indexes, autoIncrement)
  }
object Entity {
//  def apply
//    [ T : TypeTag ]
//    ( uniqueKeys    : Set[Seq[String]]  = Set.empty,
//      indexes       : Set[Seq[String]]  = Set.empty,
//      autoIncrement : Set[String]       = Set.empty )
//    : Entity[T]
//    = new Entity[T](
//        primaryKey = Seq("id"),
//        autoIncrement = autoIncrement + "id",
//        uniqueKeys = uniqueKeys,
//        indexes = indexes
//      )
}
