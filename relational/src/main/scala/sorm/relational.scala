package sorm
package object relational {

import org.joda.time._
import java.sql.{Types => jdbcTypes}
import reflect.runtime.{universe => ru}

/**
 * A parameter for PreparedStatement.
 * Required only for setting the prepared statement, so 
 */
case class Value(value: Any, t: JDBCType)

type JDBCType = Int
object JDBCType {
  import java.sql.Types._
  def detect(a: Any) = a match {
    case null           => Some(NULL)
    case _ : String     => Some(VARCHAR)
    case _ : BigDecimal => Some(DECIMAL)
    case _ : Boolean    => Some(BIT)
    case _ : Byte       => Some(TINYINT)
    case _ : Short      => Some(SMALLINT)
    case _ : Int        => Some(INTEGER)
    case _ : Long       => Some(BIGINT)
    case _ : Float      => Some(REAL)
    case _ : Double     => Some(DOUBLE)
    case _ : LocalDate  => Some(DATE)
    case _ : LocalTime  => Some(TIME)
    case _ : DateTime   => Some(TIMESTAMP)
    case _              => None
  }
  val varchar   = VARCHAR
  val time      = TIME
  val date      = DATE
  val timestamp = TIMESTAMP
  val integer   = INTEGER
  val double    = DOUBLE
  val float     = FLOAT
  val clob      = CLOB
  val bigint    = BIGINT
  val boolean   = BOOLEAN
  val decimal   = DECIMAL
  val smallint  = SMALLINT
  val tinyint   = TINYINT
}

// FIXME: mappings are not bijective. E.g., 
// a Scala String may be mapped to VARCHAR or BLOB depending on extra conditions.
trait MappingToJDBCType[a]{ val jdbcType: JDBCType }
implicit val i0 = new MappingToJDBCType[String]{ val jdbcType = jdbcTypes.VARCHAR }




}
