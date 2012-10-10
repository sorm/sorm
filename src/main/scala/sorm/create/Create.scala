package sorm.create

import sorm._
import mappings._
import jdbc._
import sext.Sext._

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
            = m.containedTableMappings.foldLeft( Vector(m) ){ (q, m) ⇒
                m match {
                  case m : SlaveTableMapping ⇒
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