package vorm.structure

import vorm._
import reflection._
import ddl._
import select._
import structure._
import extensions._

package object mapping {

  def columnsForContainerTable
    ( m : Mapping ) 
    : Iterable[Column]
    = m match {
        case m : CollectionTableMapping ⇒ 
          Nil
        case m : TableMapping ⇒ 
          m.primaryKeyColumns
            .view
            .map{ c ⇒ 
              c.copy(
                autoIncrement = false,
                name = m.columnName + "$" + c.name
              )
            }
        case m : HasChildren ⇒ 
          m.children.view flatMap columnsForContainerTable
        case m : ValueMapping ⇒
          m.column :: Nil
      }


}