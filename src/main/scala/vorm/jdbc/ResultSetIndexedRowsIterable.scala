package vorm.jdbc

import java.sql.ResultSet
import vorm._
import extensions._

class ResultSetIndexedRowsIterable
  ( rs : ResultSet ) 
  extends Iterable[IndexedSeq[Any]]
  {
    type JdbcType = Int

    private val md = rs.getMetaData

    private val indexTypeSeq : IndexedSeq[(Int, JdbcType)]
      = ( 1 to md.getColumnCount ) zipBy md.getColumnType

    def iterator = new Iterator[IndexedSeq[Any]] {
      rs.beforeFirst()

      def hasNext = !rs.isLast
      def next = {
        rs.next()
        indexTypeSeq.map{ case (i, t) ⇒ rs.value(i, t) }
      }
    }
  }
