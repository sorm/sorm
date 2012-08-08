package vorm

import com.weiglewilczek.slf4s.Logger
import java.sql.{ResultSet, PreparedStatement, Connection, Statement => JStatement}

import vorm._
import extensions._

package object jdbc {
  private lazy val logger = Logger(this.getClass)

  implicit def connectionAdapter(x: Connection) = new ConnectionAdapter(x)
  implicit def preparedStatementAdapter(x: PreparedStatement) = new PreparedStatementAdapter(x)
  implicit def resultSetAdapter(x: ResultSet) = new ResultSetAdapter(x)


  type JdbcType = Int

  case class JdbcValue
    ( value : Any, 
      t : JdbcType )
  
  case class Statement
    ( sql: String,
      data: Seq[JdbcValue] = Nil ) 
    {
      override def toString
        = "Statement:\n" + 
          ( "Sql:\n" + sql.indent(2) + "\n" +
            "Data:\n" + data.view.map{_.value}.mkString(", ") )
            .indent(2)
    }

}