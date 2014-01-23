package sorm.relational.sql

import sorm._, core._

/**
 * SQL templates AST.
 */
object templates {

  sealed trait Statement
  object Statement {
    // case class Insert
    //   ( table: String, columns: Seq[String] )
    case class Select
      ( what: What,
        from: From,
        where: Option[CondExpr] = None,
        groupBy: Seq[WhatExpr] = Nil,
        having: Option[CondExpr] = None,
        orderBy: Seq[OrderByExpr] = Nil,
        // Should be dynamic
        limit: Option[Int] = None,
        offset: Option[Int] = None,
        distinct: Boolean = false )
      extends Statement
    case class Union
      ( left: Statement, right: Statement )
      extends Statement
  }

  case class What( head: WhatExpr, tail: Seq[WhatExpr] )

  sealed trait WhatExpr
  object WhatExpr {
    case class Ref( ref: templates.Ref ) extends WhatExpr
    case class AllColumns( table: Option[String] ) extends WhatExpr
    case class Count( expr: WhatExpr, distinct: Boolean ) extends WhatExpr
  }

  /**
   * In case of a column it's a column name and a table name or alias.
   */
  case class Ref( name: String, context: Option[String] ) 

  case class From( expr: FromExpr, as: Option[String], joins: Seq[Join] )

  sealed trait FromExpr
  object FromExpr {
    case class Table( name: String ) extends FromExpr
  }

  case class Join
    ( expr: FromExpr, 
      as: Option[String], 
      on: CondExpr, 
      kind: JoinKind )

  sealed trait JoinKind
  object JoinKind {
    case object Left extends JoinKind
  }

  /**
   * A conditional expression.
   */
  sealed trait CondExpr

  case class OrderByExpr( what: Ref, desc: Boolean = false )

}

object values {
  sealed trait Statement
  object Statement {
    case class Select
      extends Statement
  }
}
