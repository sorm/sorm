package vorm.structure

sealed case class EntitySettings
  ( primaryKey    : Seq[String] = Seq.empty,
    uniqueKeys    : Set[Seq[String]] = Set.empty,
    indexes       : Set[Seq[String]] = Set.empty,
    autoIncrement : Set[String] = Set.empty )
