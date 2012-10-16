package sorm.jdbc

import sorm._
import sext._

case class Statement
  ( sql: String,
    data: Seq[JdbcValue] = Nil )
