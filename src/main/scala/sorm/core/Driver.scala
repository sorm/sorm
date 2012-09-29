package sorm.core

import sext.Sext._

import sorm._
import abstractSql._
import abstractSql.AbstractSql._

trait Driver {
  def statement ( asql : Statement ) : jdbc.Statement
}