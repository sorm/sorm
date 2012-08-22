package vorm.sql

import vorm._
import extensions._

object Sql {

  sealed trait Sql

  sealed trait Statement extends Sql

  sealed trait WhatObject 
  sealed trait FromObject
  sealed trait GroupByObject
  sealed trait WhereObject
  sealed trait HavingObject
  sealed trait JoinObject

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


  sealed trait Condition [ T ] extends Sql
  sealed case class CompositeCondition [ T ] 
    ( operator : CompositeOperator, left : Condition, right : Condition )
    extends Condition[T]
  sealed case class BinaryCondition [ T ] 
    ( operator : BinaryOperator, left : T, right : T )
    extends Condition[T]
  sealed case class UnaryCondition [ T ] 
    ( operator : UnaryOperator, left : T, right : T )
    extends Condition[T]

  sealed trait CompositeOperator extends Sql
  object CompositeOperator {
    case object And extends CompositeOperator
    case object Or extends CompositeOperator
  }

  sealed trait UnaryOperator extends Sql
  object UnaryOperator {
    case object IsNull extends UnaryOperator
    case object IsNotNull extends UnaryOperator
  }

  sealed trait BinaryOperator extends Sql
  object BinaryOperator {
    case object Equals extends BinaryOperator
    case object NotEquals extends BinaryOperator
    case object Larger extends BinaryOperator
    case object LargerOrEquals extends BinaryOperator
    case object Smaller extends BinaryOperator
    case object SmallerOrEquals extends BinaryOperator
    case object Like extends BinaryOperator
    case object NotLike extends BinaryOperator
    case object Regex extends BinaryOperator
    case object NotRegex extends BinaryOperator
    case object In extends BinaryOperator
    case object NotIn extends BinaryOperator
  }

}