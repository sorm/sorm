package vorm.structure

sealed case class EntitySettings
  ( indexes       : Set[Seq[String]] = Set.empty,
    uniqueKeys    : Set[Seq[String]] = Set.empty )
