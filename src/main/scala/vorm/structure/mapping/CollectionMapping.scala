package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

abstract class CollectionMapping
  extends TableMapping
  {
    def uniqueKeyColumns : Set[Seq[Column]] = Set.empty
    def indexColumns : Set[Seq[Column]] = Set.empty

    lazy val containerTableMappingForeignKey : Option[ForeignKey]
      = containerTableMapping map { t ⇒
          ForeignKey(
            t.tableName,
            t.primaryKeyColumns.map{ c ⇒ ("p$" + c.name) → c.name },
            ForeignKey.ReferenceOption.Cascade
          )
        }

    lazy val containerTableColumns : IndexedSeq[Column]
      = containerTableMapping
          .view
          .flatMap{_.primaryKeyColumns}
          .map{ c => c.copy(name = "p$" + c.name, autoIncrement = false) }
          .toIndexedSeq

    lazy val foreignKeys : Map[TableMapping, ForeignKey]
      = nestedTableMappingsForeignKeys ++
        containerTableMappingForeignKey.map{ containerTableMapping.get → _ }

  }
