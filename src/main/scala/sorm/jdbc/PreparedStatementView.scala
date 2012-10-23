package sorm.jdbc

import java.sql.PreparedStatement
import org.joda.time._

import sorm._
import joda.Extensions._
import sext._, embrace._

class PreparedStatementView
  ( s : PreparedStatement ) 
  {
    /**
     * @see <a href=http://docstore.mik.ua/orelly/java-ent/servlet/ch09_02.htm#ch09-22421>jdbc table
     */
    def set 
      ( i : Int, 
        v : Any ) 
      {
        v match {
          case v: Boolean              => s.setBoolean(i, v)
          case v: String               => s.setString(i, v)
          case v: Byte                 => s.setByte(i, v)
          case v: Short                => s.setShort(i, v)
          case v: Int                  => s.setInt(i, v)
          case v: Long                 => s.setLong(i, v)
          case v: Float                => s.setFloat(i, v)
          case v: Double               => s.setDouble(i, v)
          case v: BigDecimal           => s.setBigDecimal(i, v.bigDecimal)
          case v: LocalDate            => s.setDate(i, v.toJava)
          case v: LocalTime            => s.setTime(i, v.toJava)
          case v: DateTime             => s.setTimestamp(i, v.toJava)
          case null                    => s.setNull(i, java.sql.Types.NULL)
          case _                       => ???
        }
      }
    def set 
      ( i : Int,
        v : JdbcValue )
      {
        set( i, v.value )
      }

//    /**
//     * @see <a href=http://docstore.mik.ua/orelly/java-ent/servlet/ch09_02.htm#ch09-22421>jdbc table
//     */
//    def set
//      ( i : Int,
//        v : Any,
//        t : JdbcType )
//      {
//        import java.sql.Types._
//        t match {
//          case CHAR | VARCHAR =>
//            s.setString(i, v.asInstanceOf[String])
//          case LONGVARCHAR =>
//            s.setString(i, v.asInstanceOf[String])
//          // case LONGVARCHAR =>
//          //   s.setCharacterStream(i, v.asInstanceOf[java.io.Reader])
//          case NUMERIC | DECIMAL =>
//            s.setBigDecimal(i, v.asInstanceOf[java.math.BigDecimal])
//          case BIT =>
//            s.setBoolean(i, v.asInstanceOf[Boolean])
//          case TINYINT =>
//            s.setByte(i, v.asInstanceOf[Byte])
//          case SMALLINT =>
//            s.setShort(i, v.asInstanceOf[Short])
//          case INTEGER =>
//            s.setInt(i, v.asInstanceOf[Int])
//          case BIGINT =>
//            s.setLong(i, v.asInstanceOf[Long])
//          case REAL =>
//            s.setFloat(i, v.asInstanceOf[Float])
//          case FLOAT | DOUBLE =>
//            s.setDouble(i, v.asInstanceOf[Double])
//          case BINARY | VARBINARY =>
//            s.setBytes(i, v.asInstanceOf[Array[Byte]])
//          case LONGVARBINARY =>
//            s.setBinaryStream(i, v.asInstanceOf[java.io.InputStream])
//          case DATE =>
//            s.setDate(i, v.asInstanceOf[LocalDate].toJava)
//          case TIME =>
//            s.setTime(i, v.asInstanceOf[LocalTime].toJava)
//          case TIMESTAMP =>
//            s.setTimestamp(i, v.asInstanceOf[DateTime].toJava)
//          case BLOB =>
//            s.setBlob(i, v.asInstanceOf[java.sql.Blob])
//          case NULL =>
//            s.setNull(i, NULL)
//          case _ => ???
//        }
//      }
  }
