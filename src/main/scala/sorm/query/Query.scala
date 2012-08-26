package sorm.query

import sorm._
import reflection._
import structure._
import mapping._
import extensions.Extensions._

object Query {

  sealed case class Query
    ( kind    : Kind,
      mapping : TableMapping,
      where   : Option[Where] = None,
      order   : Seq[Order] = Nil,
      limit   : Option[Int] = None,
      offset  : Int = 0 )

  trait Kind
  object Kind {
    case object Select extends Kind
    case object Count extends Kind
  }

  sealed trait Where
  
  sealed trait Composite 
    extends Where {
      def left : Where
      def right : Where
    }
  sealed case class And
    ( left : Where,
      right : Where )
    extends Composite
  sealed case class Or
    ( left : Where,
      right : Where )
      extends Composite

  sealed case class Filter
    ( operator : Operator,
      mapping : Mapping,
      value : Any )
    extends Where


  sealed trait Operator
  object Operator {
    case object Equals extends Operator
    case object NotEquals extends Operator
    case object Larger extends Operator
    case object LargerIncluding extends Operator
    case object Smaller extends Operator
    case object SmallerIncluding extends Operator
    case object Like extends Operator
    case object Regex extends Operator
    case object In extends Operator
    case object Contains extends Operator
    /**
     * Makes part of a collection
     */
    case object Constitutes extends Operator
    /**
     * Includes a collection
     */
    case object Includes extends Operator
    /**
     * For collections. Could be replaced by reference to `size` pseudo-
     * property, as well as there could be introduced such common properties as
     * `keys` and `values` for maps.
     */
    case object HasSize extends Operator
  }

  case class Order
    ( mapping : Mapping,
      reverse : Boolean = false )

}