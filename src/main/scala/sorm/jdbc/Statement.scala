package sorm.jdbc

import com.weiglewilczek.slf4s.Logger
import java.sql.{ResultSet, PreparedStatement, Connection, Statement => JStatement}
import org.joda.time.DateTime

import sorm._
import extensions._

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
