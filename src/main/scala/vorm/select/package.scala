package vorm

import extensions._

package object select {
  
  def sql
    ( node : SelectNode )
    : String
    = {

      case class Join
        ( name      : String,
          alias     : String,
          leftAlias : String,
          mappings  : Set[(String, String)] )

      def unfold
        ( node        : SelectNode,
          columns     : List[String]    = Nil,
          joins       : List[Join]      = Nil,
          parentAlias : Option[String]  = None,
          index       : Int             = 0 )
        : ( List[String], List[Join], Int )
        = {
          val alias
            = "t" + index
          val columns1 
            = node.columns.map(alias + "." + _)
          val join 
            = parentAlias.map {
                parentAlias =>
                  Join (
                    name = node.table,
                    alias = alias,
                    leftAlias = parentAlias,
                    mappings = node.mappings
                  )
              }

          (columns1 ::: columns, join ++: joins, index + 1) →
          node.children foldRight {
            case (child, (columns, joins, index))
              ⇒ unfold(child, columns, joins, Some(alias), index)
          }
        }

      val (columns, joins, _)
        = unfold(node)


      "SELECT " + columns.mkString(", ") + "\n" +
      "FROM " + node.table + " AS t0" + "\n" +
      joins
        .map {
          j ⇒ "LEFT JOIN " + j.name + " AS " + j.alias + " ON " + 
              j.mappings
                .map {
                  case (right, left)
                    ⇒ j.leftAlias + "." + left + " = " + j.alias + "." + right
                }
                .mkString(" AND ")
        }
        .mkString("\n")
    }

}