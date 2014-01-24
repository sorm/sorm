package sorm.relational.sql

import sorm._, core._, relational._

/**
 * A general compiler interface. 
 * It's granular, so that the drivers are able to override individual methods.
 *
 * Follows a rule that the values are placed in tuples isomorphic to according AST symbols,
 * with exception that single-item symbols simply forward their internals.
 */
// NOTE: Since all the compilation happens to dynamic types, 
// typeclasses wouldn't really have helped here.
trait Compiler {
  import sql.{templates => t}

  case class Output( template: String, f: F )
  
  type F = Any => Seq[Value]
  object F {
    val void: F = _ => Nil
    val id: F = { case a: Value => Seq(a) }
    def tuple2( f1: F, f2: F ): F = 
      { case (v1, v2) => f1(v1) ++ f2(v2) }
    def tuple3( f1: F, f2: F, f3: F ): F = 
      { case (v1, v2, v3) => f1(v1) ++ f2(v2) ++ f3(v3) }
    def tuple4( f1: F, f2: F, f3: F, f4: F ): F = 
      { case (v1, v2, v3, v4) => f1(v1) ++ f2(v2) ++ f3(v3) ++ f4(v4) }
    def tuple8( f1: F, f2: F, f3: F, f4: F, f5: F, f6: F, f7: F, f8: F ): F =
      { case (v1, v2, v3, v4, v5, v6, v7, v8) =>
        f1(v1) ++ f2(v2) ++ f3(v3) ++ f4(v4) ++ f5(v5) ++ f6(v6) ++ f7(v7) ++ f8(v8)
      }
    def itr( fs: Iterable[F] ): F = 
      { case a: Iterable[Any] =>
        val noF = bug("Not enough functions")
        val noValues = bug("Not enough values")
        fs.zipAll(a, noF, noValues).flatMap{ case (a, b) => a(b) }.toSeq
      }
  }

  def compile( a: t.Statement ): Output = a match {
    case a: t.Statement.Insert => compile(a)
    case a: t.Statement.Update => compile(a)
    case a: t.Statement.Delete => compile(a)
    case a: t.Statement.Select => compile(a)
    case a: t.Statement.Union => compile(a)
  }
  def compile( a: t.Statement.Insert ): Output = {
    val table = compile(a.table)
    val (columns, exprs) = {
      val (columns, exprs) = a.values.toStream.unzip
      (columns.map(compile), exprs.map(compile))
    }
    val template = {
      val valuesT = 
        if( columns.isEmpty ) " VALUES (DEFAULT)"
        else {
          val columnsT = columns.map(_.template).mkString(", ")
          val exprsT = exprs.map(_.template).mkString(", ")
          s"($columnsT) VALUES ($exprsT)"
        }
      "INSERT INTO " + table.template + valuesT
    }
    val f = F.tuple3(table.f, F.itr(columns.map(_.f)), F.itr(exprs.map(_.f)))
    Output(template, f)
  } 
  def compile( a: t.Statement.Update ): Output = {
    val table = compile(a.table)
    val values = a.values.toStream.map{ case (a, b) => (compile(a), compile(b)) }
    val where = a.where.map(compile)
    val template = {
      val setExprs = {
        if( values.isEmpty ) bug("No values for update")
        val setExprs = values.map{ case (id, expr) => id.template + " = " + expr.template }
        setExprs.mkString(", ")
      }
      val whereT = where.map(" WHERE " + _.template).mkString
      "UPDATE " + table.template + " SET " + setExprs + whereT
    }
    val f = {
      val valuesF = {
        val fs = values.map{ case (id, expr) => F.tuple2(id.f, expr.f) }
        F.itr(fs)
      }
      F.tuple3(table.f, valuesF, F.itr(where.map(_.f)))
    }
    Output(template, f)
  } 
  def compile( a: t.Statement.Delete ): Output = {
    val from = compile(a.from)
    val where = a.where.map(compile)
    val limit = a.limit.map(compile)
    val offset = a.offset.map(compile)
    val template = {
      val fromT = " " + from.template
      val whereT = where.map(" WHERE " + _.template).mkString
      val limitT = limit.map(" LIMIT " + _.template).mkString
      val offsetT = offset.map(" OFFSET " + _.template).mkString
      "DELETE" + fromT + whereT + limitT + offsetT
    }
    val f = F.tuple4(from.f, F.itr(where.map(_.f)), F.itr(limit.map(_.f)), F.itr(offset.map(_.f)))
    Output(template, f)
  } 
  def compile( a: t.Statement.Select ): Output = {
    val what = a.what.toStream.map(compile)
    val from = compile(a.from)
    val where = a.where.map(compile)
    val groupBy = a.groupBy.toStream.map(compile)
    val having = a.having.map(compile)
    val orderBy = a.orderBy.toStream.map(compile)
    val limit = a.limit.map(compile)
    val offset = a.offset.map(compile)
    val template = {
      val selectT = "SELECT " + (if( a.distinct ) " DISTINCT" else "")
      val whatT = " " + what.map(_.template).mkString(",")
      val fromT = " " + from.template
      val whereT = where.map(" WHERE " + _.template).mkString
      val groupByT = 
        if( groupBy.isEmpty ) "" 
        else " GROUP BY " + groupBy.map(_.template).mkString(", ")
      val havingT = having.map(" HAVING " + _.template).mkString
      val orderByT = 
        if( orderBy.isEmpty ) "" 
        else " ORDER BY " + groupBy.map(_.template).mkString(", ")
      val limitT = limit.map(" LIMIT " + _.template).mkString
      val offsetT = offset.map(" OFFSET " + _.template).mkString
      selectT + whatT + fromT + whereT + groupByT + havingT + orderByT + limitT + offsetT
    }
    val f = F.tuple8( F.itr(what.map(_.f)), from.f, F.itr(where.map(_.f)),
                      F.itr(groupBy.map(_.f)), F.itr(having.map(_.f)), 
                      F.itr(orderBy.map(_.f)), F.itr(limit.map(_.f)), 
                      F.itr(offset.map(_.f)) )
    Output(template, f)
  }
  def compile( a: t.Statement.Union ): Output = {
    val left = compile(a.left)
    val right = compile(a.right)
    val template = s"(${left.template}) UNION (${compile(a.right)})"
    val f = F.tuple2(left.f, right.f)
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
    val f = F.itr(table.map(_.f))
    Output(template, f)
  }
  def compile( a: t.WhatExpr.Count ): Output = {
    val what = a.what.map(compile)
    val template = {
      val whatT = what.map(_.template).mkString(", ")
      "COUNT(" + (if( a.distinct ) "DISTINCT " else "") + whatT + ")"
    }
    val f = F.itr(what.map(_.f))
    Output(template, f)
  }
  def compile( a: t.From ): Output = {
    val expr = compile(a.expr)
    val as = a.as.map(compile)
    val joins = a.joins.toStream.map(compile)
    val template = {
      val exprT = expr.template
      val asT = as.map(_.template).map(" AS " + _).getOrElse("")
      val joinsT = joins.map(_.template).map(" " + _).mkString
      "FROM " + exprT + asT + joinsT
    }
    val f = F.tuple3(expr.f, F.itr(as.map(_.f)), F.itr(joins.map(_.f)))
    Output(template, f)
  }
  def compile( a: t.FromExpr ): Output = a match {
    case a: t.FromExpr.Table => compile(a)
  }
  def compile( a: t.FromExpr.Table ): Output = compile(a.name)
  def compile( a: t.Join ): Output = {
    val dec = compile(a.declaration)
    val what = compile(a.what)
    val as = a.as.map(compile)
    val on = a.on.map(compile)
    val template = {
      dec.template + " " + what.template +
      as.map(" AS " + _.template).mkString +
      on.map(" ON " + _.template).mkString
    }
    val f = F.tuple4(dec.f, what.f, F.itr(as.map(_.f)), F.itr(on.map(_.f)))
    Output(template, f)
  }
  def compile( a: t.JoinDeclaration ): Output = a match {
    case t.JoinDeclaration.Left => Output("LEFT JOIN", F.void)
  }
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
    val f = F.tuple2(left.f, right.f)
    Output(template, f)
  }
  def compile( a: t.Condition.Comparison ): Output = {
    val operator = compile(a.operator, a.negative)
    val left = compile(a.left)
    val right = compile(a.right)
    val template = left.template + " " + operator.template + " " + right.template
    val f = F.tuple3(left.f, operator.f, right.f)
    Output(template, f)
  }
  def compile( a: t.Condition.IsNull ): Output = {
    val expr = compile(a.expr)
    val template = expr.template + " IS" + (if( a.negative ) " NOT" else "") + " NULL"
    val f = expr.f
    Output(template, f)
  }
  def compile( a: (t.Operator, Boolean) ): Output = a match {
    case (t.Operator.Equal, false) => Output("=", F.void)
    case (t.Operator.Equal, true) => Output("<>", F.void)
    case (t.Operator.Larger, false) => Output(">", F.void)
    case (t.Operator.Larger, true) => Output("<=", F.void)
    case (t.Operator.Smaller, false) => Output("<", F.void)
    case (t.Operator.Smaller, true) => Output(">=", F.void)
    case (t.Operator.Like, false) => Output("LIKE", F.void)
    case (t.Operator.Like, true) => Output("NOT LIKE", F.void)
    case (t.Operator.Regexp, false) => Output("REGEXP", F.void)
    case (t.Operator.Regexp, true) => Output("NOT REGEXP", F.void)
    case (t.Operator.In, false) => Output("IN", F.void)
    case (t.Operator.In, true) => Output("NOT IN", F.void)
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
  def compile( a: t.Placeholder ) = Output("?", F.id)
  def compile( a: t.Ref ): Output = {
    val context = a.context.map(compile)
    val symbol = compile(a.symbol)
    val template = context.map(_.template + ".").getOrElse("") + symbol.template
    val f = F.tuple2(F.itr(context.map(_.f)), symbol.f)
    Output(template, f)
  }
  def compile( a: t.Identifier ) = {
    val template = "\"" + a.value + "\""
    Output(template, F.void)
  }
  def compile( a: t.OrderByExpr ): Output = {
    val ref = compile(a.ref)
    val template = ref.template + ( if( a.desc ) " DESC" else "" )
    Output(template, ref.f)
  }

}
