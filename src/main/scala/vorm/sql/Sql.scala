package vorm.sql

import vorm._
import extensions._

object Sql {

  trait ToSql[T] {
    def toSql ( t : T ) : Sql
  }

  def alias ( x : Int ) = ( 97 + x ).toChar.toString


  trait Sql

  trait Statement extends Sql

  case class Union
    ( left : Statement,
      right : Statement )
    extends Statement

  case class Select
    ( what : Seq[SelectObject],
      from : From,
      join : Seq[Join] = Nil,
      where : Option[Clause] = None,
      groupBy : Seq[GroupByObject] = Nil,
      having : Option[Clause] = None,
      orderBy : Seq[OrderByClause] = Nil,
      limit : Option[Int] = None,
      offset : Option[Int] = None )
    extends Statement
    with FromObject 
    with JoinObject


  case class OrderByClause
    ( what : Column,
      desc : Boolean = false )
    extends Sql

  trait SelectObject extends Sql
  trait GroupByObject extends Sql

  case class Table
    ( name : String )
    extends FromObject 
    with JoinObject

  case class From
    ( what : FromObject,
      as : Option[String] = None )
    extends Sql

  trait FromObject extends Sql

  case class Join
    ( what : JoinObject,
      as : Option[String] = None,
      on : Seq[(Column, Column)] = Nil,
      kind : JoinKind = JoinKind.Left )
    extends Sql

  trait JoinObject extends Sql

  trait JoinKind
  object JoinKind {
    case object Left  extends JoinKind
    case object Right extends JoinKind
    case object Inner extends JoinKind
  }

  case class Column
    ( name : String,
      table : Option[String] = None )
    extends Sql
    with SelectObject 
    with ConditionObject
    with GroupByObject

  case class Count
    ( what : Seq[Column],
      distinct : Boolean = false )
    extends Sql
    with SelectObject 
    with ConditionObject
    with GroupByObject


  // trait Condition
  // case class CompositeCondition
  //   ( operator : CompositeOperator,
  //     left : _,
  //     right : _ )
  // case class BinaryCondition
  //   ( operator : BinaryOperator,
  //     )
  // case class UnaryCondition
  //   ( operator : UnaryOperator 
  //     )


  trait Condition
  case class Or
    ( left : Condition,
      right : Condition )
    extends Condition
  case class Equals
    ( left : ConditionObject, right : ConditionObject )
    extends Condition
  case class IsNull
    ( what : ConditionObject )
    extends Condition


  trait ConditionObject
  case class ColumnConditionObject ( column : Column ) extends ConditionObject
  case class CountConditionObject ( count : Count ) extends ConditionObject
  case class ValueConditionObject ( value : Any ) extends ConditionObject





  // case class Condition
  //   ( operator : Operator,
  //     left : ConditionObject,
  //     right : ConditionObject )
  //   extends Sql
  //   with ConditionObject

  // trait ConditionObject


  // trait Operator
  // object Operator {
  //   case object Equals extends Operator
  //   case object NotEquals extends Operator
  //   case object Larger extends Operator
  //   case object LargerIncluding extends Operator
  //   case object Smaller extends Operator
  //   case object SmallerIncluding extends Operator
  //   case object Like extends Operator
  //   case object Regex extends Operator
  //   case object In extends Operator
  //   case object Contains extends Operator
  // }

  // trait Clause extends Sql

  // trait CompositeClause extends Clause

  // case class And
  //   ( left : Clause,
  //     right : Clause )
  //   extends CompositeClause

  // case class Or
  //   ( left : Clause,
  //     right : Clause )
  //   extends CompositeClause
  

  // case class Condition 
  //   ( operator : Operator, 
  //     left : ConditionObject,
  //     right : ConditionObject )

  // trait ConditionObject extends Sql

  //   abstract class Composite 
  //     ( operator : String )
  //     extends Clause 


  //   abstract class Condition 
  //     ( operator : String )
  //     extends Clause 
  //     {
  //       def left : ConditionObject
  //       def right : ConditionObject
  //       def rendering
  //         = left.rendering + " " + operator + " " + right.rendering
  //       def data
  //         = left.data ++ right.data
  //     }
    
  //   case class Equals 
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition(
  //       right match {
  //         case Value(null) => "IS"
  //         case _ => "="
  //       }
  //     )
    
  //   case class NotEquals
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition(
  //       right match {
  //         case Value(null) => "IS NOT"
  //         case _ => "!="
  //       }
  //     )

  //   case class Larger
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition(">")

  //   case class LargerIncluding
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition(">=")

  //   case class Smaller
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition("<")

  //   case class SmallerIncluding
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition("<=")

  //   case class Like
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition("LIKE")

  //   case class Regex
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition("REGEXP")
    
  //   case class In
  //     ( left : ConditionObject,
  //       right : ConditionObject )
  //     extends Condition("IN")
  // }

  // case class Value
  //   ( value : Any )
  //   extends ConditionObject
  //   {
  //     def rendering
  //       = "?"
  //     def data
  //       = Vector(value)
  //   }



}