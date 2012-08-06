package vorm

import vorm._
import structure._
import mapping._
import ddl._
import extensions._

package object create {

  def ddl
    ( ms : Iterable[TableMapping] )
    : String
    = ms.view
        .foldLeft( Vector.empty[Table] ){ case (ts, m) ⇒
          def nestedTablesQueue
            ( m : TableMapping )
            : Seq[Table]
            = m.nestedTableMappings.toSeq.view
                .flatMap{ nestedTablesQueue } :+
                m.table
          ts ++ nestedTablesQueue( m )
        }
        .distinct
        .map{_.ddl}
        .mkString("\n")
}