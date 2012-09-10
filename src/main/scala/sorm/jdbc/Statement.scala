package sorm.jdbc

import sorm._
import sext.Sext._

case class Statement
  ( sql: String,
    data: Seq[JdbcValue] = Nil )
