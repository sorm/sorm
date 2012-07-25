package vorm

import extensions._
import query._
import structure._
import reflection._

package object selectAbstraction {

  // case class Select
  //   ( task : Task,
  //     mapping : mapping.Table,
  //     clause : Option[SelectionNode] = None,
  //     rows : Option[Int] = None )
  // extends SelectionNode

  // trait Task
  // object Task {
  //   case object RootPrimaryKey extends Task
  //   case object PrimaryKey extends Task
  //   case object ResultSet extends Task
  // }

  trait Clause
  object Clause {

    trait Composite extends Clause {
      def left : Clause
      def right : Clause
    }

    case class And
      ( left : Clause,
        right : Clause )
      extends Composite

    case class Or
      ( left : Clause,
        right : Clause )
      extends Composite

    trait Filter extends Clause {
      def mapping : Mapping
    }

    case class Select
      ( mapping : structure.mapping.Table,
        clause : Option[Clause] = None,
        rows : Option[Int] = None )
      extends Filter

    case class Equals
      ( mapping : structure.mapping.Value,
        value : Any )
      extends Filter

  }

    


}