package vorm.sql

import vorm._
import extensions._

object NewSql {
  
  sealed trait Sql

  sealed trait Statement 
    extends Sql

  case class Union 
    ( left  : Statement,  
      right : Statement ) 
    extends Statement
    with FromObject 
    with JoinObject

  case class Select
    ( what : Seq[WhatObject],
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

  sealed trait WhatObject extends Renderable

  sealed trait GroupByObject extends Renderable

  case class OrderByClause
    ( what : Column,
      desc : Boolean = false )
    extends Renderable


  case class Table
    ( name : String )
    extends FromObject 
    with JoinObject

  case class From
    ( what : FromObject,
      as : Option[String] = None )

  sealed trait FromObject extends Renderable

  case class Join
    ( what : JoinObject,
      as : Option[String] = None,
      on : Seq[(Column, Column)] = Nil,
      kind : JoinKind = JoinKind.Left )

  sealed trait JoinObject extends Renderable

  sealed trait JoinKind
  object JoinKind {
    case object Left  extends JoinKind
    case object Right extends JoinKind
    case object Inner extends JoinKind
  }

  case class Column
    ( name : String,
      table : Option[String] = None )
    extends SelectObject 
    with ConditionObject
    with GroupByObject

  case class Count
    ( what : Seq[Column],
      distinct : Boolean = false )
    extends SelectObject 
    with ConditionObject
    with GroupByObject


  trait Clause extends Renderable
  trait ConditionObject extends Renderable


}