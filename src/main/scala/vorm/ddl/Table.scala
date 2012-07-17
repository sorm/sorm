package vorm.ddl

sealed case class Table
  ( name : String,
    columns : Seq[Column],
    primaryKey : Seq[String],
    uniqueKeys : Set[Seq[String]] = Set.empty,
    indexes : Set[Seq[String]] = Set.empty,
    foreignKeys : Set[ForeignKey] = Set.empty )
