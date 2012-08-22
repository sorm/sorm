package vorm.sql

import vorm._
import extensions._

object Sql {

  sealed trait Sql

  sealed trait Statement extends Sql

  sealed trait WhatObject extends Sql
  sealed trait FromObject extends Sql
  sealed trait GroupByObject extends Sql
  sealed trait WhereObject extends Sql
  sealed trait HavingObject extends Sql
  sealed trait JoinObject extends Sql

  sealed case class Union
    ( left : Statement,
      right : Statement )
    extends Statement

  sealed case class Select
    ( what : Seq[WhatObject],
      from : From,
      join : Seq[Join] = Nil,
      where : Option[Condition[WhereObject]] = None,
      groupBy : Seq[GroupByObject] = Nil,
      having : Option[Condition[HavingObject]] = None,
      orderBy : Seq[OrderBy] = Nil,
      limit : Option[Int] = None,
      offset : Option[Int] = None )
    extends Statement
    with FromObject 
    with JoinObject

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

  sealed case class Count
    ( what : Seq[Column],
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


  sealed trait Condition[T] extends Sql
  sealed case class Or[T]
    ( left : Condition[T], right : Condition[T] )
    extends Condition[T]
  sealed case class And[T]
    ( left : Condition[T], right : Condition[T] )
    extends Condition[T]
  sealed case class IsNull[T]
    ( what : T )
    extends Condition[T]
  sealed case class IsNotNull[T]
    ( what : T )
    extends Condition[T]
  sealed case class Comparison[T]
    ( operator : Operator, left : T, right : T )
    extends Condition[T]

  sealed trait Operator extends Sql
  object Operator {
    case object Equals extends Operator
    case object NotEquals extends Operator
    case object Larger extends Operator
    case object LargerOrEquals extends Operator
    case object Smaller extends Operator
    case object SmallerOrEquals extends Operator
    case object Like extends Operator
    case object NotLike extends Operator
    case object Regex extends Operator
    case object NotRegex extends Operator
    case object In extends Operator
    case object NotIn extends Operator
  }

}