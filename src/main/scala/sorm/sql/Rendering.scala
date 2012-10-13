package sorm.sql

import sorm._
import sext._

import Sql._

object Rendering {

  trait Renderable {
    def template : String
    def data : Seq[Any]
  }

}