package vorm

import com.weiglewilczek.slf4s.Logger
import java.sql.{ResultSet, PreparedStatement, Connection, Statement => JStatement}
import org.joda.time.DateTime

import vorm._
import extensions._

package object jdbc {
  private lazy val logger = Logger(this.getClass)

  implicit def connectionAdapter(x: Connection) = new ConnectionAdapter(x)
  implicit def preparedStatementAdapter(x: PreparedStatement) = new PreparedStatementAdapter(x)
  implicit def resultSetAdapter(x: ResultSet) = new ResultSetAdapter(x)


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
          case _ : DateTime   => ???
          case null           => NULL
          case _              => ???
        }
  }

  case class JdbcValue
    ( value : Any, 
      t : JdbcType )
  object JdbcValue {
    def apply ( v : Any ) : JdbcValue = apply(v, JdbcType(v))
  }
  

}