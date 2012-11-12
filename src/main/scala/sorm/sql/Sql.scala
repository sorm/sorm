package sorm.sql

sealed trait Sql
object Sql {

  case class Insert
    ( table : String, columns : Seq[String], values : Seq[Any] )
    extends Sql

  case class Update
    ( table : String, exprs : Seq[SetExpression], where : Option[Where] )
    extends Sql

  case class SetExpression
    ( column : Column, value : Any )
    extends Sql

  case class Delete
    ( table : String, where : Option[Where] )
    extends Sql

  case class Where
    ( condition : Condition[WhereObject] )
    extends Sql
    

  sealed trait Statement
    extends Sql
    with FromObject
    with JoinObject

  sealed trait WhatObject extends Sql
  sealed trait FromObject extends Sql
  sealed trait JoinObject extends Sql
  sealed trait WhereObject extends Sql
  sealed trait GroupByObject extends Sql
  sealed trait HavingObject extends Sql
  sealed trait CountObject extends Sql

  sealed case class Union
    ( left : Statement,
      right : Statement )
    extends Sql
    with Statement

  sealed case class Select
    ( what : Seq[WhatObject],
      from : From,
      join : Seq[Join] = Nil,
      where : Option[Condition[WhereObject]] = None,
      groupBy : Seq[GroupByObject] = Nil,
      having : Option[Condition[HavingObject]] = None,
      orderBy : Seq[OrderBy] = Nil,
      limit : Option[Int] = None,
      offset : Option[Int] = None,
      distinct : Boolean = false )
    extends Sql
    with Statement

  sealed case class OrderBy
    ( what : Column,
      desc : Boolean = false )
    extends Sql

  sealed case class Table
    ( name : String )
    extends Sql 
    with FromObject 
    with JoinObject

  sealed case class From
    ( what : FromObject,
      as : Option[String] = None )
    extends Sql


  sealed case class Join
    ( what : JoinObject,
      as : Option[String] = None,
      on : Seq[(Column, Column)] = Nil,
      kind : JoinKind = JoinKind.Left )
    extends Sql


  sealed trait JoinKind
  object JoinKind {
    case object Left  extends JoinKind
    case object Right extends JoinKind
    case object Inner extends JoinKind
  }

  sealed case class Column
    ( name : String,
      table : Option[String] = None )
    extends Sql
    with WhatObject 
    with WhereObject
    with HavingObject
    with GroupByObject
    with CountObject

  sealed case class AllColumns
    ( table : Option[String] = None )
    extends Sql
    with WhatObject
    with GroupByObject
    with CountObject

  sealed case class Count
    ( what : Seq[CountObject],
      distinct : Boolean = false )
    extends Sql
    with WhatObject 
    with HavingObject
    with GroupByObject

  sealed case class Value
    ( value : Any )
    extends Sql
    with WhereObject
    with HavingObject


  sealed trait Condition 
    [ T <: Sql ] 
    extends Sql

  sealed case class CompositeCondition 
    [ T <: Sql ] 
    ( left : Condition[T], right : Condition[T], operator : CompositeOperator )
    extends Condition[T]

  sealed case class IsNull 
    [ T <: Sql ] 
    ( what : T, negative : Boolean = false )
    extends Condition[T]

  sealed case class Comparison
    [ T <: Sql ] 
    ( left : T, right : T, operator : ComparisonOperator )
    extends Condition[T]


  sealed trait CompositeOperator extends Sql
  case object And extends CompositeOperator
  case object Or extends CompositeOperator

  sealed trait ComparisonOperator extends Sql
  case object Equal extends ComparisonOperator
  case object NotEqual extends ComparisonOperator
  case object Larger extends ComparisonOperator
  case object LargerOrEqual extends ComparisonOperator
  case object Smaller extends ComparisonOperator
  case object SmallerOrEqual extends ComparisonOperator
  case object Like extends ComparisonOperator
  case object NotLike extends ComparisonOperator
  case object Regexp extends ComparisonOperator
  case object NotRegexp extends ComparisonOperator
  case object In extends ComparisonOperator
  case object NotIn extends ComparisonOperator

}