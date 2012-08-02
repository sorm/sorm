package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

abstract class CollectionTableMapping
  extends TableMapping
  {
    def foreignKeyForOwnerTable = None

    lazy val ownerTableForeignKey
      = ownerTable map { t ⇒ 
          ForeignKey(
            t.tableName,
            t.primaryKeyColumns.map{ c ⇒ ("p_" + c.name) → c.name },
            ForeignKey.ReferenceOption.Cascade
          )
        }

//    lazy val parentKeyColumns
//      = ownerTable.flatMap {
//          _.primaryKeyColumns.view
//            .map(_.column)
//            .map(c => c copy (name = "p_" + c.name, autoIncremented = false))
//            .zipWithIndex
//            .map(MappedColumn.ParentKeyPart(_, _, this))
//        }
  }
