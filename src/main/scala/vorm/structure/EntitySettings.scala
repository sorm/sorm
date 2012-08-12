package vorm.structure

sealed case class EntitySettings
  ( uniqueKeys    : Set[Seq[String]] = Set.empty,
    indexes       : Set[Seq[String]] = Set.empty )
