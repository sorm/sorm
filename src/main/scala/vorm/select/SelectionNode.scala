package vorm.select



case class SelectionNode 
  ( from : mapping.Table
    )

object SelectionNode {
  trait Kind
  object Kind {
    case object RootPrimaryKey extends Kind
    case object ResultSet extends Kind
  }
}