package vorm.sql

import vorm._
import extensions._

import NewSql._
import Rendering._

object StandardRendering {

  private def quote
    ( s : String )
    = s

  implicit class UnionRenderable
    [ L <: Statement <% Renderable,
      R <: Statement <% Renderable ]
    ( self : Union[L, R] )
    {
      def template 
        ( self : Union[L, R] )
        = "( " + self.left.template.indent(2).trim + " )\n" +
          "UNION\n" +
          "( " + self.right.template.indent(2).trim + " )\n" 
      def data
        ( self : Union[L, R] )
        = self.left.data ++: self.right.data ++: Stream()
    }

}