package vorm.create

import vorm._
import structure._
import mapping._
import jdbc._
import extensions._

object Create {

  def ddl
    ( ms : Iterable[TableMapping] )
    : String
    = statements(ms)
        .map{_.sql + ";"}
        .mkString("\n\n")

  def statements
    ( ms : Iterable[TableMapping] )
    = ms.foldLeft( Vector.empty[TableMapping] ){ (q, m) ⇒
          def queue
            ( m : TableMapping )
            : Vector[TableMapping]
            = m.nestedTableMappings.foldLeft( Vector(m) ){ (q, m) ⇒
                m match {
                  case m : CollectionMapping ⇒
                    q ++ queue(m)
                  case m ⇒
                    queue(m) ++ q
                }
              }
          q ++ queue( m )
        }
        .toStream
        .map{ _.table }
        .distinct
        .map{_.ddl}
        .map{Statement(_)}
}