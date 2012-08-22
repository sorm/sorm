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

}