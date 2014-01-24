package sorm.relational.sql

import sorm._, core._, relational._

/**
 * A general compiler interface. 
 * It's granular, so that the drivers are able to override individual methods.
 */
// NOTE: Since all the compilation happens to dynamic types, 
// typeclasses wouldn't really have helped here.
trait Compiler {
  import sql.{templates => t}

  case class Output( template: String, f: F )
  type F = Any => Seq[Value]

  protected val voidF: F = _ => Nil
  protected def pairF( left: Output, right: Output ): F = { case (a, b) => left.f(a) ++ right.f(b) }
  protected def seqF( fs: Seq[F] ): F = { case a: Seq[Any] =>
    val noF = bug("Not enough functions")
    val noValues = bug("Not enough values")
    fs.zipAll(a, noF, noValues).flatMap{ case (a, b) => a(b) }
  }

  def compile( a: t.Statement ): Output = a match {
    case a: t.Statement.Insert => compile(a)
    case a: t.Statement.Update => compile(a)
    case a: t.Statement.Delete => compile(a)
    case a: t.Statement.Select => compile(a)
    case a: t.Statement.Union => compile(a)
  }
  def compile( a: t.Statement.Insert ) = ??? : Output
  def compile( a: t.Statement.Update ) = ??? : Output
  def compile( a: t.Statement.Delete ) = ??? : Output
  def compile( a: t.Statement.Select ): Output = {
    val from = compile(a.from)
    val where = a.where.map(compile)
    val template = {
      val distinct = if( a.distinct ) " DISTINCT" else ""
      val whereT = where.map("WHERE " +)
      ???
    }
    ???
  }
  def compile( a: t.Statement.Union ): Output = {
    val left = compile(a.left)
    val right = compile(a.right)
    val template = s"(${left.template}) UNION (${compile(a.right)})"
    val f = pairF(left, right)
    Output(template, f)
  }
  def compile( a: t.What ): Output = {
    val exprs = (a.head +: a.tail.toStream).map(compile)
    val template = exprs.map(_.template).mkString(", ")
    val f: F = seqF(exprs.map(_.f))
    Output(template, f)
  }
  def compile( a: t.WhatExpr ): Output = a match {
    case a: t.WhatExpr.Ref => compile(a)
    case a: t.WhatExpr.AllColumns => compile(a)
    case a: t.WhatExpr.Count => compile(a)
  }
  def compile( a: t.WhatExpr.Ref ): Output = compile(a.ref)
  def compile( a: t.WhatExpr.AllColumns ): Output = {
    val table = a.table.map(compile)
    val template = table.map(_.template).map(_ + ".").getOrElse("") + "*"
    val f = table.map(_.f).getOrElse(voidF)
    Output(template, f)
  }
  def compile( a: t.WhatExpr.Count ): Output = ???
  def compile( a: t.From ): Output = {
    val expr = compile(a.expr)
    val as = a.as.map(compile)
    val joins = a.joins.toStream.map(compile)
    val template = {
      val exprT = expr.template
      val asT = as.map(_.template).map(" AS " +).getOrElse("")
      val joinsT = joins.map(_.template).map(" " +).mkString
      "FROM " + exprT + asT + joinsT
    }
    val f: F = { case (a, b, c: Seq[Any]) =>
      val exprV = expr.f(a)
      val asV = as.map(_.f(b)).getOrElse(Nil)
      val joinsV = seqF(joins.map(_.f))(c)
      exprV ++ asV ++ joinsV
    }
    Output(template, f)
  }
  def compile( a: t.FromExpr ): Output = a match {
    case a: t.FromExpr.Table => compile(a)
  }
  def compile( a: t.FromExpr.Table ): Output = compile(a.name)
  def compile( a: t.Join ): Output = ???
  def compile( a: t.Condition ): Output = a match {
    case a: t.Condition.Fork => compile(a)
    case a: t.Condition.Comparison => compile(a)
    case a: t.Condition.IsNull => compile(a)
  }
  def compile( a: t.Condition.Fork ): Output = {
     val left = compile(a.left)
     val right = compile(a.right)
     val template = {
      a.or match {
        case true => s"(${left.template} OR ${right.template})"
        case false => s"${left.template} AND ${right.template}"
      }
     }
     Output(template, pairF(left, right))
  }
  def compile( a: t.Condition.Comparison ): Output = {
    val operator = compile(a.operator, a.negative)
    val left = compile(a.left)
    val right = compile(a.right)
    val template = left.template + " " + operator.template + " " + right.template
    val f: F = { case (a, b) => left.f(a) ++ right.f(b) }
    Output(template, f)
  }
  def compile( a: t.Condition.IsNull ): Output = {
    val expr = compile(a.expr)
    val template = expr.template + " IS" + (if( a.negative ) " NOT" else "") + " NULL"
    val f = expr.f
    Output(template, f)
  }
  def compile( a: (t.Operator, Boolean) ): Output = a match {
    case (t.Operator.Equal, false) => Output("=", voidF)
    case (t.Operator.Equal, true) => Output("<>", voidF)
    case (t.Operator.Larger, false) => Output(">", voidF)
    case (t.Operator.Larger, true) => Output("<=", voidF)
    case (t.Operator.Smaller, false) => Output("<", voidF)
    case (t.Operator.Smaller, true) => Output(">=", voidF)
    case (t.Operator.Like, false) => Output("LIKE", voidF)
    case (t.Operator.Like, true) => Output("NOT LIKE", voidF)
    case (t.Operator.Regexp, false) => Output("REGEXP", voidF)
    case (t.Operator.Regexp, true) => Output("NOT REGEXP", voidF)
    case (t.Operator.In, false) => Output("IN", voidF)
    case (t.Operator.In, true) => Output("NOT IN", voidF)
  }
  def compile( a: t.Expr ): Output = a match {
    case a: t.Expr.Placeholder => compile(a)
    case a: t.Expr.Constant => compile(a)
    case a: t.Expr.Select => compile(a)
    case a: t.Expr.Ref => compile(a)
  }
  def compile( a: t.Expr.Placeholder ): Output = compile(a.placeholder)
  def compile( a: t.Expr.Constant ) = ???
  def compile( a: t.Expr.Select ) = ???
  def compile( a: t.Expr.Ref ): Output = compile(a.ref)
  def compile( a: t.Placeholder ) = Output("?", voidF)
  def compile( a: t.Ref ): Output = {
    val context = a.context.map(compile)
    val symbol = compile(a.symbol)
    val template = context.map(_.template + ".").getOrElse("") + symbol.template
    Output(template, voidF)
  }
  def compile( a: t.Identifier ) = {
    val template = "\"" + a.value + "\""
    Output(template, voidF)
  }

}
