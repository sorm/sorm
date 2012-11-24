package sorm.jdbc

import sorm._
import core.SormException
import java.sql.{ResultSet, PreparedStatement, Connection, Statement => JStatement}
import org.joda.time._

import sext._, embrace._

object `package` {

  implicit def connectionAdapter(x: Connection) = new JdbcConnection(x)
  implicit def preparedStatementAdapter(x: PreparedStatement) = new PreparedStatementView(x)
  implicit def resultSetAdapter(x: ResultSet) = new ResultSetView(x)


  type JdbcType = Int
  object JdbcType {
    import java.sql.Types._
    /**
     * Shouldn't really be used
     */
    def apply ( v : Any )
      = v match {
          case _ : String     => VARCHAR
          case _ : BigDecimal => DECIMAL
          case _ : Boolean    => BIT
          case _ : Byte       => TINYINT
          case _ : Short      => SMALLINT
          case _ : Int        => INTEGER
          case _ : Long       => BIGINT
          case _ : Float      => REAL
          case _ : Double     => DOUBLE
          case _ : LocalDate  => DATE
          case _ : LocalTime  => TIME
          case _ : DateTime   => TIMESTAMP
          case null           => NULL
          case _              => throw new SormException("Value of unsupported type `" + v.getClass + "`: " + v)
        }
  }

  case class JdbcValue
    ( value : Any,
      t : JdbcType )
  object JdbcValue {
    def apply ( v : Any ) : JdbcValue = apply(v, JdbcType(v))
  }


  case class Statement
    ( sql: String,
      data: Seq[JdbcValue] = Nil )
  object Statement {
    def simple ( sql : String, data : Seq[Any] = Nil )
      = Statement(sql, data.map(JdbcValue(_)))
  }
}