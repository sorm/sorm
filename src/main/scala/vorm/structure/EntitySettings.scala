package vorm.structure

sealed case class EntitySettings
  ( primaryKey      : Option[Seq[String]],
    uniqueKeys      : Set[Seq[String]],
    indexes         : Set[Seq[String]],
    autoIncrements  : Set[String] )
