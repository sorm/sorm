package sorm.sql

import sorm._
import extensions.Extensions._

import Sql._
import Rendering._

object StandardRendering {

  private def quote
    ( s : String )
    = s

  implicit class RenderableSql
    ( self : Sql )
    extends Renderable
    {
      def template
        = self match {
            case Union(l, r) =>
              "( " + l.template.indent(2).trim + " )\n" +
              "UNION\n" +
              "( " + r.template.indent(2).trim + " )\n"
            case Select(what, from, join, where, groupBy, having, orderBy, limit, offset) =>
              "SELECT\n" +
              ( what.view.map{_.template}.mkString(", ") +
                "\n" + from.template +
                join
                  .view
                  .map{_.template}
                  .mkString("\n")
                  .satisfying{ ! _.isEmpty }
                  .map{"\n" + _}
                  .getOrElse("") +
                where
                  .map{_.template}
                  .map{ "\nWHERE " + _.indent("WHERE ".length).trim }
                  .getOrElse("") +
                groupBy
                  .view
                  .map{_.template}
                  .mkString(", ")
                  .satisfying{ ! _.isEmpty }
                  .map{ "\nGROUP BY " + _ }
                  .getOrElse("") +
                having
                  .map{_.template}
                  .map{ "\nHAVING " + _.indent("HAVING ".length).trim }
                  .getOrElse("") +
                orderBy
                  .map{_.template}
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
              what.template + ( if( desc ) " DESC" else "" )
            case Table(name) =>
              quote(name)
            case From(what, as) =>
              "FROM\n" +
              ( ( what match {
                    case Table(name) ⇒ quote(name)
                    case s : Sql ⇒ "(\n" + s.template.indent(2) + "\n)"
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
                    case s : Sql ⇒ "( " + s.template.indent(2).trim + " )"
                  } ) +
                as.map{ "\nAS " + quote(_) }
                  .getOrElse("") +
                on.map{ case (l, r) ⇒ l.template + " = " + r.template }
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
              what.view.map{ _.template }.mkString(", ") +
              ")"
            case Value(_) =>
              "?"
            case CompositeCondition(l, r, o) =>
              def subConditionSql
                ( c : Condition[_] )
                : String
                = c match {
                    case c : CompositeCondition[_] =>
                      "( " + c.template.indent("( ".length).trim + " )"
                    case c =>
                      c.template
                  }
              subConditionSql(l) + " " +
              o.template + "\n" +
              subConditionSql(r)
            case IsNull(what, negative) =>
              what.template +
              " IS" + ( if( negative ) " NOT" else "" ) + " NULL"
            case Comparison(l, r, o) =>
              l.template + " " + o.template + " " + r.template
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

      def data
        = self match {
            case Union(l, r) => 
              l.data ++: r.data
            case Select(what, from, join, where, groupBy, having, orderBy, 
                        limit, offset) =>
              ( what ++: from +: join ++: where ++: groupBy ++: having ++:
                orderBy 
              ) .view
                .flatMap{_.data}
                .toStream
            case OrderBy(what, desc) => 
              what.data
            case From(what, as) => 
              what.data
            case Join(what, _, _, _) => 
              what.data
            case Value(v) => 
              Stream(v)
            case CompositeCondition(l, r, _) => 
              l.data ++: r.data
            case Comparison(l, r, _) => 
              l.data ++: r.data
            case _ => 
              Stream()
          }
    }

}