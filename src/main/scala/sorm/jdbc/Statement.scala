package sorm.jdbc

import sorm._
import extensions.Extensions._

case class Statement
  ( sql: String,
    data: Seq[JdbcValue] = Nil )
  {
    override def toString
      = "Statement:\n" +
        ( sql + "\n" +
          ( if( data.nonEmpty )
              data.view.map{_.value}.mkString(", ")
            else "" ) )
          .indent(2)
  }
