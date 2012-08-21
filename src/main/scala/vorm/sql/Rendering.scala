package vorm.sql

import vorm._
import extensions._

import NewSql._

object Rendering {

  trait Rendering[T <: Sql] {
    def template ( self : T ) : String
    def data ( self : T ) : Seq[Any]
  }

  trait RenderingSyntax {
    implicit class SqlExtensions 
      [ T <: Sql : Rendering ] 
      ( self : T ) 
      {
        def template
          = implicitly[Rendering[T]].template(self)
        def data
          = implicitly[Rendering[T]].data(self)
      }
  }

  // case class Rendering
  //   ( template : String, 
  //     data : Seq[Any] )


}