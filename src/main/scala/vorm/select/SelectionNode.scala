package vorm.select

import vorm._
import reflection._
import structure._
import query._

import SelectionNode._

trait SelectionNode

object SelectionNode {


  case class Statement
    ( select : sql.Select, 
      results : Seq[(ddl.Column, mapping.Table)] )

  // def statement
  //   ( n : SelectionNode )
  //   : Statement
  //   = n match {
  //       case n : Select
  //         ⇒ 
  //     }

  // def andNode
  //   ( s : Statement,
  //     n : SelectionNode )
  //   : Statement
  //   = n match {
  //       case And( left, right )
  //         ⇒ 
  //       case n : Select
  //         ⇒ ???
  //     }




  case class Select
    ( task : Task,
      mapping : mapping.Table,
      clause : Option[SelectionNode] = None,
      rows : Option[Int] = None )
  extends SelectionNode

  trait Task
  object Task {
    case object RootPrimaryKey extends Task
    case object PrimaryKey extends Task
    case object ResultSet extends Task
  }


  trait Composite extends SelectionNode {
    def left : SelectionNode
    def right : SelectionNode
  }

  case class And
    ( left : SelectionNode,
      right : SelectionNode )
    extends Composite

  case class Or
    ( left : SelectionNode,
      right : SelectionNode )
    extends Composite


  trait Filter extends SelectionNode {
    def mapping : mapping.Value
    def value : Any
  }

  case class Equals
    ( mapping : mapping.Value,
      value : Any )
    extends Filter

}