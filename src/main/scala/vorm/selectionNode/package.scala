package vorm

import extensions._
import query._
import structure._
import reflection._

package object selectionNode {


  case class Statement
    ( sql : sql.Select, 
      // results : Seq[(ddl.Column, mapping.Table)] )
      results : Seq[ResultSetColumn] )


  // def statement
  //   ( n : SelectionNode )
  //   : Statement
  //   = n match {
  //       case n : Select
  //         ⇒ 
  //     }

  // def withNode
  //   ( s : Statement,
  //     n : SelectionNode )
  //   : Statement
  //   = n match {
  //       case SelectionNode.And( left, right )
  //         ⇒ ???
  //       case SelectionNode.Select( t, m, c, None )
  //         ⇒ ???
  //       case n : SelectionNode.Select
  //         ⇒ ???
  //     }


  def andNode
    ( s : Statement,
      n : SelectionNode )
    : Statement
    = n match {
        case n : SelectionNode.Select
          ⇒ s.copy(
                sql
                  = s.sql.copy(
                        join
                          = s.sql.join :+
                            sql.Join(
                                what
                                  = select(n),
                                from
                                  = 
                              )
                      )
              )
      }


    


}