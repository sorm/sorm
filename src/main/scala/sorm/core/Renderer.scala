package sorm.core

import sext.Sext._

import sorm._
import abstractSql._
import abstractSql.AbstractSql._

trait Renderer {
  def sql ( asql : Statement ) : jdbc.Statement
}