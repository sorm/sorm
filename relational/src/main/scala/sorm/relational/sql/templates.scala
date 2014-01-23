package sorm.relational.sql

import sorm._, core._, relational._

/**
 * SQL templates AST.
 */
object templates {

  sealed trait Statement
  object Statement {
    case class Insert
      ( table: Identifier, columns: Seq[Identifier] )
      extends Statement
    case class Update
      ( table: Identifier, setExprs: Seq[(Ref, Expr)], where: Option[Condition] )
      extends Statement
    case class Delete
      ( table: Identifier, 
        where: Option[Condition],
        limit: Option[IntOrPlaceholder] = None,
        offset: Option[IntOrPlaceholder] = None )
      extends Statement
    case class Select
      ( what: What,
        from: From,
        where: Option[Condition] = None,
        groupBy: Seq[WhatExpr] = Nil,
        having: Option[Condition] = None,
        orderBy: Seq[OrderByExpr] = Nil,
        limit: Option[IntOrPlaceholder] = None,
        offset: Option[IntOrPlaceholder] = None,
        distinct: Boolean = false )
      extends Statement
    case class Union
      ( left: Statement, right: Statement )
      extends Statement
  }

  case class Identifier( value: String )

  case class What( head: WhatExpr, tail: Seq[WhatExpr] )

  sealed trait WhatExpr
  object WhatExpr {
    case class Ref( ref: templates.Ref ) extends WhatExpr
    case class AllColumns( table: Option[Identifier] ) extends WhatExpr
    case class Count( expr: WhatExpr, distinct: Boolean ) extends WhatExpr
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
    ( expr: FromExpr, 
      as: Option[Identifier], 
      on: Condition, 
      kind: JoinKind )

  sealed trait JoinKind
  object JoinKind {
    case object Left extends JoinKind
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

  case class OrderByExpr( what: Ref, desc: Boolean = false )

  sealed trait IntOrPlaceholder
  object IntOrPlaceholder {
    case class Int( value: scala.Int ) extends IntOrPlaceholder
    case class Placeholder( placeholder: templates.Placeholder ) extends IntOrPlaceholder
  }

}
