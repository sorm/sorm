package sorm.structure

import sorm._
import reflection._
import ddl._
import structure._
import sext.Sext._

package object mapping {

  //  TODO : should be just moved to Mapping as a property
  def columnsForContainerTable
    ( m : Mapping ) 
    : Iterable[Column]
    = m match {
        case m : CollectionMapping ⇒
          Nil
        case m : TableMapping ⇒ 
          m.primaryKeyColumns
            .view
            .map{ c ⇒ 
              c.copy(
                autoIncrement
                  = false,
                name
                  = m.columnName + "$" + c.name,
                nullable
                  = m.membership
                      .toInstanceOf[Some[Membership.EntityProperty]].isDefined
              )
            }
        case m : HasChildren ⇒ 
          m.children.view flatMap columnsForContainerTable
        case m : ColumnMapping ⇒
          m.column :: Nil
      }


}