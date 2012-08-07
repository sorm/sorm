package vorm.jdbc

import collection.mutable.ListBuffer
import java.sql._

import vorm._
import extensions._

class ResultSetAdapter
  ( rs : ResultSet ) 
  {

    def indexedRowsIterator 
      = new Iterator[IndexedSeq[Any]] {
          rs.beforeFirst()

          val md 
            = rs.getMetaData

          val indexTypeSeq : IndexedSeq[(Int, JdbcType)]
            = ( 1 to md.getColumnCount ) zipBy md.getColumnType

          def hasNext 
            = !rs.isLast

          def next = {
            rs.next()
            indexTypeSeq.map{ case (i, t) ⇒ rs.value(i, t) }
          }
        }

    def parseAndClose() 
      = {
        val r = indexedRowsIterator.toList
        rs.close()
        r
      }

    @deprecated("use parseAndClose()")
    def parseToListsAndClose() 
      = parseAndClose()

    @deprecated("index based approach is preferred")
    def parseToMapsAndClose() 
      = {
        val b = ListBuffer[Map[String, Any]]()

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
        t match {
          case CHAR | VARCHAR     => rs.getString(i)
          case LONGVARCHAR        => rs.getCharacterStream(i)
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
          case _                  => ???
        }
      }

    def value
      ( name : String, t : JdbcType )
      : Any 
      = {
        import Types._
        t match {
          case INTEGER => rs.getInt(name)
          case BIGINT => rs.getLong(name)
          case CHAR | VARCHAR => rs.getString(name)
          case _ => ???
        }
      }


  }