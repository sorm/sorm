package sorm.create

import sorm._
import mappings._
import jdbc._
import sext.Sext._

object Create {

  def tables
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
}