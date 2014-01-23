package sorm.relational.sql

import sorm._, core._, relational._

object values {
  sealed trait Statement
  object Statement {
    case class Select
      extends Statement
    case class Insert extends Statement
  }
}
