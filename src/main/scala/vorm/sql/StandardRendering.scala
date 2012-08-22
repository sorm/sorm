package vorm.sql

import vorm._
import extensions._

import NewSql._
import Rendering._

object StandardRendering extends RendererSyntax {

  private def quote
    ( s : String )
    = s

  implicit val standardSqlRenderer
    = new Renderer[Sql] {
        def template
          ( self : Sql )
          = self match {
              case Union(l, r) =>
                "( " + template(l).indent(2).trim + " )\n" +
                "UNION\n" +
                "( " + template(r).indent(2).trim + " )\n"
              case Select(what, from, join, where, groupBy, having, orderBy, limit, offset) =>
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
            }

        def data
          ( self : Sql )
          = self match {
              case Union(l, r) =>
                data(l) ++: data(r) ++: Stream()
              case Select(what, from, join, where, groupBy, having, orderBy, limit, offset) =>
                ( what ++: from +: join ++: where ++: groupBy ++: having ++:
                  orderBy ++: Nil )
                  .flatMap{data}
            }
      }

}