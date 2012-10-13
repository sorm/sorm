package sorm.abstractSql

/**
 * Abstracts away from table aliases and from-join management, using a table
 * reference which represents a specific table in a query and information on
 * how it should be joined
 */
object AbstractSql {

  
  sealed trait Statement

  sealed case class Union
    ( left : Statement, right : Statement )
    extends Statement

  sealed case class Intersection
    ( left : Statement, right : Statement )
    extends Statement

  sealed case class Select
    ( expressions : Seq[Column],
      condition : Option[Condition] = None,
      having : Seq[HavingCount] = Nil,
      groupBy : Seq[Column] = Nil,
      order : Seq[Order] = Nil,
      limit : Option[Int] = None,
      offset : Int = 0 )
    extends Statement

  // //  As a wrapper for any statement idea
  // sealed case class HavingCount
  //   ( statement : Statement,
  //     count : Int )
  //   extends Statement


  sealed case class HavingCount
    ( table : Table,
      column : String,
      operator : Operator,
      count : Int )


  case class Order
    ( table : Table,
      column : String,
      reverse : Boolean = false )


  sealed case class Column
    ( name : String,
      table : Table )


  sealed trait Condition

  sealed case class And
    ( left : Condition, right : Condition )
    extends Condition
  
  sealed case class Or
    ( left : Condition, right : Condition )
    extends Condition

  sealed case class Comparison
    ( table : Table,
      column : String,
      operator : Operator,
      value : Any )
    extends Condition

  case object EverTrue extends Condition

  case object EverFalse extends Condition


  sealed case class Table
    ( name : String,
      parent : Option[Parent] = None )
    

  sealed case class Parent
    ( table : Table,
      bindings : Seq[(String, String)] )
  

  sealed trait Operator
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