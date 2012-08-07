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

    def value
      ( i : Int, t : Int ) 
      : Any 
      = {
        import Types._
        t match {
          case INTEGER => rs.getInt(i)
          case BIGINT => rs.getLong(i)
          case CHAR | VARCHAR => rs.getString(i)
          case _ => ???
        }
      }

    def value
      ( name : String, t : Int )
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