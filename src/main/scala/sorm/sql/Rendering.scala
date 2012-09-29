package sorm.sql

import sext.Sext._

import sorm._
import jdbc._
import Sql._

object Rendering {

  trait Renderable {
    def template : String
    def data : Seq[Any]
    def statement = Statement(template, data map JdbcValue.apply)
  }

}