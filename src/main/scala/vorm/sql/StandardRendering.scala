package vorm.sql

import vorm._
import extensions._

import NewSql._
import Rendering._

object StandardRendering extends RenderingSyntax {

  private def quote
    ( s : String )
    = s

  implicit def unionRendering
    [ L <: Statement : Rendering,
      R <: Statement : Rendering ]
    = new Rendering[Union[L, R]] {
        def template 
          ( self : Union[L, R] )
          = "( " + implicitly[Rendering[L]].template(self.left).indent(2).trim + " )\n" +
            "UNION\n" +
            "( " + implicitly[Rendering[R]].template(self.right).indent(2).trim + " )\n" 
        def data
          ( self : Union[L, R] )
          = implicitly[Rendering[L]].data(self.left) ++: 
            implicitly[Rendering[R]].data(self.right) ++: 
            Stream()
      }

}