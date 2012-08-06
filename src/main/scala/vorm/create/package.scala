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
    = {
      val masters
        : Map[TableMapping, Set[EntityMapping]]
        = ms.zipBy{
              def nestedEntityMappings
                ( m : Mapping )
                : Set[EntityMapping]
                = m match {
                    case m : ValueMapping ⇒ Set.empty
                    case m : EntityMapping ⇒ Set(m)
                    case m : HasChildren ⇒
                      m.children.view flatMap nestedEntityMappings toSet
                  }
              nestedEntityMappings(_)
            }
            .toMap
      ???
    }

}