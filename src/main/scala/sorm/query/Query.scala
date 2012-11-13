package sorm.query

import sorm._
import reflection._
import mappings._
import sext._, embrace._

//  in future it should be used as instructions on what to fetch, thus becoming an internal lingua for queries, a frontend abstraction over abstractSql and etc.
//  TODO:
//    - get rid of mappings, leave only string queries, delaying the mapping resolution to deeper apis
//    - introduce FetchQuery, CountQuery, ExistsQuery and Update and Insert - basically, make querying an abstraction for all mapping-oriented manipulations
sealed case class Query
  ( mapping : EntityMapping,
    where   : Option[Query.Where] = None,
    order   : Seq[Query.Order] = Nil,
    limit   : Option[Int] = None,
    offset  : Int = 0 )
object Query {

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
  case object Equal extends Operator
  case object NotEqual extends Operator
  case object Larger extends Operator
  case object LargerOrEqual extends Operator
  case object Smaller extends Operator
  case object SmallerOrEqual extends Operator
  case object Like extends Operator
  case object NotLike extends Operator
  case object Regex extends Operator
  case object NotRegex extends Operator
  case object In extends Operator
  case object NotIn extends Operator
  case object Contains extends Operator
  case object NotContains extends Operator
  /**
   * Makes part of a collection
   */
  case object Constitutes extends Operator
  case object NotConstitutes extends Operator
  /**
   * Includes a collection
   */
  case object Includes extends Operator
  case object NotIncludes extends Operator
  /**
   * For collections. Could be replaced by reference to `size` pseudo-
   * property, as well as there could be introduced such common properties as
   * `keys` and `values` for maps.
   */
  case object HasSize extends Operator

  case class Order
    ( mapping : Mapping,
      reverse : Boolean = false )

}