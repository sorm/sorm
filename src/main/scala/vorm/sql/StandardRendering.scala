package vorm.sql

import vorm._
import extensions._

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
            case Select(what, from, join, where, groupBy, having, orderBy,
                        limit, offset) =>
              ???
          }
      def data
        = self match {
            case Union(l, r) =>
              l.data ++: r.data ++: Stream()
          }
    }
  implicit val standardSqlRenderer
    = new Renderer[Sql] {
        def template
          ( self : Sql )
          = self match {
              case Union(l, r) =>
                "( " + template(l).indent(2).trim + " )\n" +
                "UNION\n" +
                "( " + template(r).indent(2).trim + " )\n"
              case Select(what, from, join, where, groupBy, having, orderBy,  limit, offset) =>
                "SELECT\n" +
                ( what.view.map{template}.mkString(", ") +
                  "\n" + template(from) +
                  join
                    .view
                    .map{template}
                    .mkString("\n")
                    .satisfying{ ! _.isEmpty }
                    .map{"\n" + _}
                    .getOrElse("") +
                  where
                    .map{template}
                    .map{ "\nWHERE " + _.indent("WHERE ".length).trim }
                    .getOrElse("") +
                  groupBy
                    .view
                    .map{template}
                    .mkString(", ")
                    .satisfying{ ! _.isEmpty }
                    .map{ "\nGROUP BY " + _ }
                    .getOrElse("") +
                  having
                    .map{template}
                    .map{ "\nHAVING " + _.indent("HAVING ".length).trim }
                    .getOrElse("") +
                  orderBy
                    .map{template}
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
              case Count(what, distinct) =>
                "COUNT(" +
                ( if( distinct ) "DISTINCT " else "" ) +
                template(what) +
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

        def data
          ( self : Sql )
          = self match {
              case Union(l, r) => 
                data(l) ++: data(r)
              case Select(what, from, join, where, groupBy, having, orderBy, limit, offset) =>
                ( what ++: from +: join ++: where ++: groupBy ++: having ++:
                  orderBy ++: Nil )
                  .flatMap{data}
              case OrderBy(what, desc) => 
                data(what)
              case Table(_) => 
                Stream()
              case From(what, as) =>
                data(what)
              case Join(what, _, _, _) =>
                data(what)
              case Column(_, _) =>
                Stream()
              case Count(_, _) =>
                Stream()
              case Value(v) =>
                Stream(v)
              case CompositeCondition(l, r, _) =>
                data(l) ++: data(r)
              case IsNull(_, _) =>
                Stream()
              case Comparison(l, r, _) =>
                data(l) ++: data(r)

            }
      }

}