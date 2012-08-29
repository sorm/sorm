package sorm.jdbc

import sorm._
import extensions.Extensions._

case class Statement
  ( sql: String,
    data: Seq[JdbcValue] = Nil )
