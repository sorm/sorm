package vorm
import extensions._
package object select {
  
  def sql
    ( node : SelectNode )
    : String
    = {

      // case class TableInstructions
      //   ( name : String,
      //     alias : String,
      //     join : Option[(String, Set[(String, String)])],
      //     columns : List[String] )


      case class Join
        ( name : String,
          alias : String,
          leftAlias : String,
          mappings : Set[(String, String)] )

      def unfold
        ( node : SelectNode,
          columns : List[String] = Nil,
          joins : List[Join] = Nil,
          parentAlias : Option[String] = None,
          index : Int = 0 )
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

      // def unfold
      //   ( node  : SelectNode,
      //     acc   : ( List[String], List[Join], Int, Option[String] )
      //           = ( Nil, Nil, 0, None ) )
      //   : ( List[String], List[Join], Int, Option[String] )
      //   = {
      //     val alias
      //       = "t" + index
      //     val columns 
      //       = node.columns.map(alias + "." + _)
      //     val join 
      //       = parentAlias.map {
      //           Join (
      //             name = node.name,
      //             alias = alias,
      //             leftAlias = _,
      //             mappings = mappings
      //           )
      //         }

      //     ( )
      //     node.children foldRight unfold

      //   }

      // def unfold
      //   ( node : SelectNode, 
      //     parentAlias : Option[String] = None
      //     acc : ( List[String], List[Join] ) )
      //   : ( List[String], List[Join] )
      //   = {
      //     val alias
      //       = "t" + acc._2.size
      //     val columns 
      //       = node.columns.map(alias + "." + _)
      //     val join 
      //       = Join (node.table, alias, )
      //     ( acc._1 )
      //   }


//      def unfold
//        ( node : SelectNode,
//          columns : List[String] = Nil,
//          joins : List[Join] = Nil )
//        : ( List[String], List[Join] )
//        = {
//          def alias = "t" + joins.size
//          ( )
//        }

      val (columns, joins, _)
        = unfold(node)


      "SELECT " + columns.mkString(", ") + "\n" +
      "FROM " + node.table + " " + "\n" +
      joins
        .map {
          j ⇒ "LEFT JOIN " + j.name + " AS " + j.alias + " ON " + 
              j.mappings
                .map {
                  case (right, left)
                    ⇒ j.leftAlias + "." + left + " = " + j.alias + "." + right
                }
                .mkString(", ")
        }
        .mkString("\n")
    }

}