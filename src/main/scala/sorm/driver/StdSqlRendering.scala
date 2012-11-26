package sorm.driver

import sext._, embrace._

import sorm._
import jdbc._
import sql._, Sql._

trait StdSqlRendering {
  def statement ( sql : Sql ) : jdbc.Statement
    = (sql $ template, sql $ data map JdbcValue.apply) $$ jdbc.Statement.apply
  protected def quote ( x : String ) : String
  protected def template ( sql : Sql ) : String
    = sql match {
        case Delete(table, where) =>
          "DELETE FROM " + quote(table) +
          ( where.map(template).map("\n" + _).getOrElse("") 
          ).indent(2)
        case Insert(table, columns, values) if columns.isEmpty && values.isEmpty =>
          "INSERT INTO " + quote(table) + " VALUES (DEFAULT)"
        case Insert(table, columns, values) =>
          "INSERT INTO " + quote(table) +
          ( "\n( " + columns.map(quote).mkString(", ") + " )" +
            "\nVALUES" +
            "\n( " + values.map(_ => "?").mkString(", ") + " )"
          ).indent(2)
        case Update(table, exps, where) =>
          "UPDATE " + quote(table) +
          ( "\nSET " + exps.ensuring(_.nonEmpty).map(template).mkString(",\n").indent(4).trim +
            where.map(template).map("\n" + _).getOrElse("")
          ).indent(2)
        case Where(exp) =>
          "WHERE " + template(exp).indent(6).trim
        case SetExpression(cr, v) =>
          template(cr) + " = ?"
        case Union(l, r) =>
          "( " + template(l).indent(2).trim + " )\n" +
          "UNION\n" +
          "( " + template(r).indent(2).trim + " )\n"
        case Select(what, from, join, where, groupBy, having, orderBy, limit, offset, distinct) =>
          "SELECT" + distinct.option(" DISTINCT").mkString + "\n" +
          ( what.view.map{template(_)}.mkString(", ") +
            "\n" + template(from) +
            join
              .view
              .map{template(_)}
              .mkString("\n")
              .satisfying{ ! _.isEmpty }
              .map{"\n" + _}
              .getOrElse("") +
            where
              .map{template(_)}
              .map{ "\nWHERE " + _.indent("WHERE ".length).trim }
              .getOrElse("") +
            groupBy
              .view
              .map{template(_)}
              .mkString(", ")
              .satisfying{ ! _.isEmpty }
              .map{ "\nGROUP BY " + _ }
              .getOrElse("") +
            having
              .map{template(_)}
              .map{ "\nHAVING " + _.indent("HAVING ".length).trim }
              .getOrElse("") +
            orderBy
              .map{template(_)}
              .mkString(", ")
              .satisfying{ ! _.isEmpty }
              .map{ "\nORDER BY " + _ }
              .getOrElse("") +
            limit
              .map{ "\nLIMIT " + _ }
              .getOrElse("") +
            offset
              .map{ "\nOFFSET " + _ }
              .getOrElse("") )
            .indent(2)
        case OrderBy(what, desc) =>
          template(what) + ( if( desc ) " DESC" else "" )
        case Table(name) =>
          quote(name)
        case From(what, as) =>
          "FROM\n" +
          ( ( what match {
                case Table(name) ⇒ quote(name)
                case s : Sql ⇒ "(\n" + template(s).indent(2) + "\n)"
              } ) +
            as.map{ "\nAS " + quote(_) }.getOrElse("")
            )
            .indent(2)
        case Join(what, as, on, kind) =>
          ( kind match {
              case JoinKind.Left ⇒ "LEFT JOIN "
              case JoinKind.Right ⇒ "RIGHT JOIN "
              case JoinKind.Inner ⇒ "INNER JOIN "
            } ) + "\n" +
          ( ( what match {
                case Table(name) ⇒ quote(name)
                case s : Sql ⇒ "( " + template(s).indent(2).trim + " )"
              } ) +
            as.map{ "\nAS " + quote(_) }
              .getOrElse("") +
            on.map{ case (l, r) ⇒ template(l) + " = " + template(r) }
              .mkString(" AND\n")
              .satisfying{ ! _.isEmpty }
              .map{ "\nON " + _.indent("ON ".length).trim }
              .getOrElse("")
            )
            .indent(2)
        case Column(name, table) =>
          table.map{quote(_) + "."}.getOrElse("") + quote(name)
        case AllColumns(table) =>
          table.map{quote(_) + "."}.getOrElse("") + "*"
        case Count(what, distinct) =>
          "COUNT(" +
          ( if( distinct ) "DISTINCT " else "" ) +
          what.view.map{ template(_) }.mkString(", ") +
          ")"
        case Value(_) =>
          "?"
        case CompositeCondition(l, r, o) =>
          def subConditionSql
            ( c : Condition[_] )
            : String
            = c match {
                case c : CompositeCondition[_] =>
                  "( " + template(c).indent("( ".length).trim + " )"
                case c =>
                  template(c)
              }
          subConditionSql(l) + " " +
          template(o) + "\n" +
          subConditionSql(r)
        case IsNull(what, negative) =>
          template(what) +
          " IS" + ( if( negative ) " NOT" else "" ) + " NULL"
        case Comparison(l, r, o) =>
          template(l) + " " + template(o) + " " + template(r)
        case And =>
          "AND"
        case Or =>
          "OR"
        case Equal =>
          "="
        case NotEqual =>
          "<>"
        case Larger =>
          ">"
        case LargerOrEqual =>
          ">="
        case Smaller =>
          "<"
        case SmallerOrEqual =>
          "<="
        case Like =>
          "LIKE"
        case NotLike =>
          "NOT LIKE"
        case Regexp =>
          "REGEXP"
        case NotRegexp =>
          "NOT REGEXP"
        case In =>
          "IN"
        case NotIn =>
          "NOT IN"
      }
  protected def data ( sql : Sql ) : Stream[Any]
    = sql match {
        case Delete(table, where) =>
          where.toStream.flatMap(data)
        case Insert(table, columns, values) =>
          values.toStream
        case Update(table, exps, where) =>
          exps.flatMap(data) ++: where.toStream.flatMap(data)
        case Where(exp) =>
          data(exp)
        case SetExpression(cr, v) =>
          v +: Stream()
        case Union(l, r) =>
          data(l) ++: data(r)
        case Select(what, from, join, where, groupBy, having, orderBy, limit, offset, distinct) =>
          ( what ++: from +: join ++: where ++: groupBy ++: having ++:
            orderBy
          ) .view
            .flatMap(data)
            .toStream
        case OrderBy(what, desc) =>
          data(what)
        case From(what, as) =>
          data(what)
        case Join(what, _, _, _) =>
          data(what)
        case Value(v) =>
          Stream(v)
        case CompositeCondition(l, r, _) =>
          data(l) ++: data(r)
        case Comparison(l, r, _) =>
          data(l) ++: data(r)
        case _ =>
          Stream()
      }
}
