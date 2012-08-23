package vorm.query

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
    ( expressions : Seq[Expression],
      condition : Option[Condition] = None,
      havingRows : Option[HavingCount] = None )
    extends Statement


  case class HavingCount
    ( table : Table,
      count : Int )


  trait Expression

  case class Column
    ( name : String,
      table : Table )


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
      operator : Sql.ComparisonOperator,
      value : Any )
    extends Condition


  case class Table
    ( name : String,
      parent : Option[Parent] )

  case class Parent
    ( table : Table,
      bindings : Seq[(String, String)] )
  


}