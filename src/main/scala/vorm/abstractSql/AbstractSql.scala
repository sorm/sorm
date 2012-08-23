package vorm.abstractSql

import vorm._
import sql._

object AbstractSql {

  
  trait Statement

  case class Union
    ( left : Statement, right : Statement )
    extends Statement

  case class Intersection
    ( left : Statement, right : Statement )
    extends Statement

  case class Select
    ( expressions : Seq[SelectExpression],
      condition : Option[Condition] = None,
      havingCount : Option[HavingCount] = None )
    extends Statement

  // //  As a wrapper for any statement idea
  // case class HavingCount
  //   ( statement : Statement,
  //     count : Int )
  //   extends Statement


  case class HavingCount
    ( table : Table,
      count : Int )


  trait SelectExpression

  case class Column
    ( name : String,
      table : Table )
    extends SelectExpression


  trait Condition

  case class And
    ( left : Condition, right : Condition )
    extends Condition
  
  case class Or
    ( left : Condition, right : Condition )
    extends Condition

  case class Comparison
    ( table : Table,
      column : String,
      operator : Operator,
      value : Any )
    extends Condition

  case class Table
    ( name : String,
      parent : Option[Parent] )

  case class Parent
    ( table : Table,
      bindings : Seq[(String, String)] )
  

  trait Operator
  case object Equal extends Operator
  case object NotEqual extends Operator
  case object Larger extends Operator
  case object LargerOrEqual extends Operator
  case object Smaller extends Operator
  case object SmallerOrEqual extends Operator
  case object Like extends Operator
  case object NotLike extends Operator
  case object Regexp extends Operator
  case object NotRegexp extends Operator
  case object In extends Operator
  case object NotIn extends Operator

}