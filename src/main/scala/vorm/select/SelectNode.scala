package vorm.select

sealed case class SelectNode
  ( table : String,
    columns : List[String],
    mappings : Set[(String, String)],
    children : List[SelectNode] )