package vorm.sql

import vorm._
import extensions._

import Sql._

object Rendering {

  trait Renderable {
    def template : String
    def data : Seq[Any]
  }

}