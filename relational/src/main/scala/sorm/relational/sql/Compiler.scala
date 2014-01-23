package sorm.relational.sql

import sorm._, core._

/**
 * A general compiler interface. 
 * It's granular, so that the drivers are able to override individual methods.
 */
// NOTE: Since all the compilation happens to dynamic types, 
// typeclasses wouldn't really have helped here.
trait Compiler extends Renderer with Arranger

protected sealed trait Renderer {
  import templates._

  def render( a: Statement ): String = a match {
    case a: Statement.Insert => render(a)
    case a: Statement.Update => render(a)
    case a: Statement.Delete => render(a)
    case a: Statement.Select => render(a)
    case a: Statement.Union => render(a)
  }
  def render( a: Statement.Insert ) = ??? : String
  def render( a: Statement.Update ) = ??? : String
  def render( a: Statement.Delete ) = ??? : String
  def render( a: Statement.Select ): String = {
    val distinct = if( a.distinct ) " DISTINCT" else ""
    val from = render(a.from)
    val where = a.where.map(render).map("WHERE " +)
    ???
    s"SELECT$distinct"
  }
  def render( a: Statement.Union ): String = {
    s"(${render(a.left)}) UNION (${render(a.right)})"
  }
  def render( a: What ): String = {
    val exprs = a.head +: a.tail
    exprs.map(render).mkString(", ")
  }
  def render( a: WhatExpr ): String = a match {
    case a: WhatExpr.Ref => render(a)
    case a: WhatExpr.AllColumns => render(a)
    case a: WhatExpr.Count => render(a)
  }
  def render( a: WhatExpr.Ref ): String = render(a.ref)
  def render( a: WhatExpr.AllColumns ): String = {
    a.table.map(render).map(_ + ".").getOrElse("") + "*"
  }
  def render( a: WhatExpr.Count ): String = ???
  def render( a: From ): String = {
    val expr = render(a.expr)
    val as = a.as.map(render).map(" AS " +).getOrElse("")
    val joins = a.joins.view.map(render).map(" " +).mkString
    "FROM " + expr + as + joins
  }
  def render( a: FromExpr ): String = a match {
    case a: FromExpr.Table => render(a)
  }
  def render( a: FromExpr.Table ): String = render(a.name)
  def render( a: Join ): String = ???
  def render( a: Condition ): String = a match {
    case a: Condition.Fork => render(a)
    case a: Condition.Comparison => render(a)
    case a: Condition.IsNull => render(a)
  }
  def render( a: Condition.Fork ): String = {
    val operator = if( a.or ) "OR" else "AND"
    "(" + render(a.left) + " " + operator + " " + render(a.right) + ")"
  }
  def render( a: Condition.Comparison ): String = {
    val operator = render(a.operator, a.negative)
    render(a.left) + " " + operator + " " + render(a.right)
  }
  def render( a: Condition.IsNull ): String = {
    render(a.expr) + " IS" + (if( a.negative ) " NOT" else "") + " NULL"
  }
  def render( a: (Operator, Boolean) ): String = a match {
    case (Operator.Equal, false) => "="
    case (Operator.Equal, true) => "<>"
    case (Operator.Larger, false) => ">"
    case (Operator.Larger, true) => "<="
    case (Operator.Smaller, false) => "<"
    case (Operator.Smaller, true) => ">="
    case (Operator.Like, false) => "LIKE"
    case (Operator.Like, true) => "NOT LIKE"
    case (Operator.Regexp, false) => "REGEXP"
    case (Operator.Regexp, true) => "NOT REGEXP"
    case (Operator.In, false) => "IN"
    case (Operator.In, true) => "NOT IN"
  }
  def render( a: Expr ): String = a match {
    case a: Expr.Placeholder => render(a)
    case a: Expr.Constant => render(a)
    case a: Expr.Select => render(a)
    case a: Expr.Ref => render(a)
  }
  def render( a: Expr.Placeholder ): String = render(a.placeholder)
  def render( a: Expr.Constant ) = ???
  def render( a: Expr.Select ) = ???
  def render( a: Expr.Ref ): String = render(a.ref)
  def render( a: Placeholder ) = "?"
  def render( a: Ref ): String = {
    val context = a.context.map(render).map(_ + ".").getOrElse("")
    val symbol = render(a.symbol) 
    context + symbol
  }
  def render( a: Identifier ) = "\"" + a.value + "\""

}

protected sealed trait Arranger {
  import values._

  def arrange( a: Statement ): Seq[Any] = a match {
    case a: Statement.Select => arrange(a)
  }
  def arrange( a: Statement.Select ) = ??? : Seq[Any]
  def arrange( a: Statement.Insert ) = ??? : Seq[Any]

}
