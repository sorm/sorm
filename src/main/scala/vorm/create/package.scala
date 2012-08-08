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
    = ms.foldLeft( Vector.empty[TableMapping] ){ (q, m) ⇒
          def queue
            ( m : TableMapping )
            : Vector[TableMapping]
            = m.nestedTableMappings.foldLeft( Vector(m) ){ (q, m) ⇒
                m match {
                  case m : CollectionTableMapping ⇒
                    q ++ queue(m)
                  case m ⇒
                    queue(m) ++ q
                }
              }
          q ++ queue( m )
        }
        .map{ _.table }
        .distinct
        .map{_.ddl + ";"}
        .mkString("\n\n")

}