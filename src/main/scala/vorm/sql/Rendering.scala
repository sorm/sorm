package vorm.sql

import vorm._
import extensions._

import NewSql._

object Rendering {

  trait Renderer[T <: Sql] {
    def template ( self : T ) : String
    def data ( self : T ) : Seq[Any]
  }

  trait RendererSyntax {
    implicit class SqlExtensions 
      [ T <: Sql : Renderer ]
      ( self : T ) 
      {
        def template
          = implicitly[Renderer[T]].template(self)
        def data
          = implicitly[Renderer[T]].data(self)
      }
  }


}