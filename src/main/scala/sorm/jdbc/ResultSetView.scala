package sorm.jdbc

import java.sql._

import sorm._
import joda.Extensions._
import sext._, embrace._
import sorm.driver.Oracle

class ResultSetView
  ( rs : ResultSet ) 
  {
    private lazy val md = rs.getMetaData
    private lazy val indexTypeSeq : IndexedSeq[(Int, JdbcType)]
      = ( 1 to md.getColumnCount ) zipBy md.getColumnType
    private lazy val indexTypeByName : Map[String, (Int, JdbcType)]
      = (1 to md.getColumnCount)
          .map(i => md.getColumnName(i).toLowerCase -> (i -> md.getColumnType(i)))
          .toMap

    /**
     * Is preferred to Iterator since not all adapters support the `isLast` test
     * required for its implementation.
     */
    def indexedRowsTraversable
      = new Traversable[IndexedSeq[Any]] {
          def foreach
            [ U ]
            ( f : IndexedSeq[Any] => U )
            {
              while( rs.next() ){
                f( indexTypeSeq.map{ case (i, t) ⇒ value(i, t) } )
              }
            }
        }
    def byNameRowsTraversable
      = new Traversable[Map[String, Any]] {
          def foreach
            [ U ]
            ( f : (Map[String, Any]) => U )
            {
              while( rs.next() ){
                indexTypeByName.map{ case (n, (i, t)) ⇒ n -> value(i, t) } $ f
              }
            }
        }

    def numeric(rs: ResultSet, i: Int): Any = {
      val converted = if (rs.getMetaData.getClass.getName.startsWith("oracle.")) Oracle.Numbers.convert(rs, i) else None
      converted.getOrElse(rs.getBigDecimal(i) $ (new BigDecimal(_)))
    }

    /**
     * @see <a href=http://docstore.mik.ua/orelly/java-ent/servlet/ch09_02.htm#ch09-22421>jdbc table
     */
    def value
      ( i : Int,
        t : JdbcType ) 
      : Any 
      = {
        import java.sql.Types._
        val r
          = t match {
              case CHAR | VARCHAR     => rs.getString(i)
              case LONGVARCHAR        => rs.getString(i)
              case NUMERIC            => numeric(rs, i)
              case DECIMAL            => rs.getBigDecimal(i) $ (new BigDecimal(_))
              case BIT                => rs.getBoolean(i)
              case TINYINT            => rs.getByte(i)
              case SMALLINT           => rs.getShort(i)
              case INTEGER            => rs.getInt(i)
              case BIGINT             => rs.getLong(i)
              case REAL               => rs.getFloat(i)
              case FLOAT | DOUBLE     => rs.getDouble(i)
              case BINARY | VARBINARY => rs.getBytes(i)
              case LONGVARBINARY      => rs.getBinaryStream(i)
              case DATE               => rs.getDate(i)
              case TIME               => rs.getTime(i)
              case TIMESTAMP | -101   => rs.getTimestamp(i) // -101 is Oracle's "timestamp with time zone"
              case BLOB               => rs.getBlob(i)
              case CLOB               => rs.getString(i)
              case BOOLEAN            => rs.getBoolean(i)
              case _                  => ???
            }
        if( rs.wasNull() ) null
        else r match {
          case r : java.sql.Date => r.toJoda
          case r : java.sql.Time => r.toJoda
          case r : java.sql.Timestamp => r.toJoda
          case _ => r
        }
      }

  }