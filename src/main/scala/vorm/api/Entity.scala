package vorm.api

sealed case class Entity
  [ T : TypeTag ]
  ( primaryKey    : Seq[String],
    uniqueKeys    : Set[Seq[String]]  = Set.empty,
    indexes       : Set[Seq[String]]  = Set.empty,
    autoIncrement : Set[String]       = Set.empty )
  {
    // here should be tests on validity of provided data
  }
object Entity {
  def apply
    [ T : TypeTag ]
    ( uniqueKeys    : Set[Seq[String]]  = Set.empty,
      indexes       : Set[Seq[String]]  = Set.empty,
      autoIncrement : Set[String]       = Set.empty )
    : Entity[T]
    = apply[T](
        primaryKey = Seq("id"),
        autoIncrement = autoIncrement + "id",
        uniqueKeys = uniqueKeys,
        indexes = indexes
      )
}
