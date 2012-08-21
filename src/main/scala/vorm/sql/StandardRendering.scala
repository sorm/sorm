package vorm.sql

import vorm._
import extensions._

import NewSql._
import Rendering._

object StandardRendering extends RenderingSyntax {

  private def quote
    ( s : String )
    = s

  implicit def sqlRendering
    = new Rendering[Sql] {
        def template 
          ( self : Sql )
          = self match {
              case Union(l, r) =>
                "( " + l.rendering.indent(2).trim + " )\n" +
                "UNION\n" +
                "( " + r.rendering.indent(2).trim + " )\n"
              case Select(what, from, join, where, groupBy, having, orderBy, limit, offset) =>
                "SELECT\n" +
                ( what.view.map{_.rendering}.mkString(", ") +
                  "\n" + from.rendering +
                  join
                    .view
                    .map{ _.rendering }
                    .mkString("\n")
                    .satisfying{ ! _.isEmpty }
                    .map{"\n" + _}
                    .getOrElse("") +
                  where
                    .map{ _.rendering }
                    .map{ "\nWHERE " + _.indent("WHERE ".length).trim }
                    .getOrElse("") +
                  groupBy
                    .view
                    .map{ _.rendering }
                    .mkString(", ")
                    .satisfying{ ! _.isEmpty }
                    .map{ "\nGROUP BY " + _ }
                    .getOrElse("") +
                  having
                    .map{ _.rendering }
                    .map{ "\nHAVING " + _.indent("HAVING ".length).trim }
                    .getOrElse("") +
                  orderBy
                    .map{ _.rendering }
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
                l.data ++: r.data ++: Stream()
              case Select(what, from, join, where, groupBy, having, orderBy, limit, offset) =>
                ( what ++: from +: join ++: where ++: groupBy ++: having ++:
                  orderBy ++: Nil )
                  .flatMap{_.data}
            }
      }

}