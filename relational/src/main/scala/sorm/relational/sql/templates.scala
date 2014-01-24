package sorm.relational.sql

import sorm._, core._, relational._

/**
 * SQL templates AST.
 */
object templates {

  sealed trait Statement
  object Statement {
    case class Insert
      ( table: Identifier, values: Seq[(Identifier, Expr)] )
      extends Statement
    case class Update
      ( table: Identifier, values: Seq[(Identifier, Expr)], where: Option[Condition] )
      extends Statement
    case class Delete
      ( from: From, 
        where: Option[Condition],
        limit: Option[Expr] = None,
        offset: Option[Expr] = None )
      extends Statement
    case class Select
      ( what: Seq[WhatExpr],
        from: From,
        where: Option[Condition] = None,
        groupBy: Seq[WhatExpr] = Nil,
        having: Option[Condition] = None,
        orderBy: Seq[OrderByExpr] = Nil,
        limit: Option[Expr] = None,
        offset: Option[Expr] = None,
        distinct: Boolean = false )
      extends Statement
    case class Union
      ( left: Statement, right: Statement )
      extends Statement
  }

  case class Identifier( value: String )

  sealed trait WhatExpr
  object WhatExpr {
    case class Ref( ref: templates.Ref ) extends WhatExpr
    case class AllColumns( table: Option[Identifier] ) extends WhatExpr
    case class Count( what: Seq[WhatExpr], distinct: Boolean ) extends WhatExpr
  }

  /**
   * In case of a column it's a column name and a table name or alias.
   */
  case class Ref( symbol: Identifier, context: Option[Identifier] ) 

  case class From( expr: FromExpr, as: Option[Identifier], joins: Seq[Join] )

  sealed trait FromExpr
  object FromExpr {
    case class Table( name: Identifier ) extends FromExpr
  }

  case class Join
    ( declaration: JoinDeclaration,
      what: FromExpr, 
      as: Option[Identifier], 
      on: Option[Condition] )

  sealed trait JoinDeclaration
  object JoinDeclaration {
    case object Left extends JoinDeclaration
  }

  /**
   * A conditional expression.
   */
  sealed trait Condition
  object Condition {
    case class Fork 
      ( left: Condition, right: Condition, or: Boolean ) 
      extends Condition
    case class Comparison
      ( left: Expr, right: Expr, operator: Operator, negative: Boolean )
      extends Condition
    case class IsNull
      ( expr: Expr, negative: Boolean )
      extends Condition
  }

  sealed trait Expr
  object Expr {
    case class Placeholder( placeholder: templates.Placeholder ) extends Expr
    case class Constant( value: Value ) extends Expr
    case class Select( select: Statement.Select ) extends Expr
    case class Ref( ref: templates.Ref ) extends Expr
  }

  sealed trait Placeholder
  case object Placeholder extends Placeholder

  sealed trait Operator
  object Operator {
    case object Equal extends Operator
    case object Larger extends Operator
    case object Smaller extends Operator
    case object Like extends Operator
    case object Regexp extends Operator
    case object In extends Operator
  }

  case class OrderByExpr( ref: Ref, desc: Boolean = false )

}
