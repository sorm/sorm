package sorm.jdbc

import java.sql._

import sorm._
import joda.Extensions._
import sext.Sext._

class ResultSetAdapter
  ( rs : ResultSet ) 
  {
    private lazy val md = rs.getMetaData
    private lazy val indexTypeSeq : IndexedSeq[(Int, JdbcType)]
      = ( 1 to md.getColumnCount ) zipBy md.getColumnType
    private lazy val indexTypeByName : Map[String, (Int, JdbcType)]
      = (1 to md.getColumnCount)
          .map(i => md.getColumnName(i) -> (i -> md.getColumnType(i)))
          .toMap

    /**
     * Is preferred to Iterator since not all adapters support the `isLast` test
     * required for its implementation.
     */
    private def indexedRowsTraversable
      = new Traversable[IndexedSeq[Any]] {
          def foreach
            [ U ]
            ( f : IndexedSeq[Any] => U )
            {
              rs.beforeFirst()
              while( rs.next() ){
                f( indexTypeSeq.map{ case (i, t) ⇒ rs.value(i, t) } )
              }
            }
        }
    private def byNameRowsTraversable
      = new Traversable[String => Any] {
          def foreach
            [ U ]
            ( f : (String => Any) => U )
            {
              rs.beforeFirst()
              while( rs.next() ){
                f( indexTypeByName andThen (value _).tupled )
              }
            }
        }

    def parse()
      = indexedRowsTraversable.toList

    def parseAndClose()
      = {
        val r = indexedRowsTraversable.toList
        rs.close()
        r
      }
    def toStream : Stream[Map[String, Any]]
      = byNameRowsTraversable.toStream map (_ => indexTypeByName.mapValues((value _).tupled))

    @deprecated("use parseAndClose()")
    def parseToListsAndClose() 
      = parseAndClose()

    @deprecated("index based approach is preferred")
    def parseToMapsAndClose() 
      = {
        val b = collection.mutable.ListBuffer[Map[String, Any]]()

        val md = rs.getMetaData

        val indexTypeByNameMap =
          (1 to md.getColumnCount)
            .map(i => md.getColumnName(i) -> (i -> md.getColumnType(i)))
            .toMap

        while( rs.next() ){
          b += indexTypeByNameMap.mapValues{ case (i, t) => value(i, t) }
        }

        rs.close()
        b.toList
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
              case NUMERIC | DECIMAL  => rs.getBigDecimal(i)
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
              case TIMESTAMP          => rs.getTimestamp(i)
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