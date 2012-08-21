package vorm.sql

import vorm._
import extensions._

import NewSql._

object Rendering {

  trait Rendering {
    def template : String
    def data : Seq[Any]
  }

}