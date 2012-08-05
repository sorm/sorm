package vorm.structure.mapping

import vorm._
import reflection._
import ddl._
import select._
import structure._
import extensions._

trait TableMapping
  extends Mapping
  with HasChildren
  {
    /**
     * First descendant table mappings
     */
    lazy val nestedTableMappings : Set[TableMapping]
      = {
        def nestedTableMappings
          ( m : Mapping )
          : Iterable[TableMapping]
          = m match {
            case m : ValueMapping ⇒ Nil
            case m : TupleMapping ⇒ m.items.view flatMap nestedTableMappings
            case m : OptionMapping ⇒ m.item as nestedTableMappings
            case m : TableMapping ⇒ m :: Nil
          }
        children.view flatMap nestedTableMappings toSet
      }

    lazy val nestedTableMappingsForeignKeys : Map[TableMapping, ForeignKey]
      = nestedTableMappings
          .view
          .collect{
            case e : EntityMapping ⇒ 
              e → 
              ForeignKey(
                e.tableName,
                e.primaryKeyColumns
                  .view
                  .map{_.name}
                  .map{ n ⇒ (e.columnName + "$" + n) → n }
                  .toList,
                ForeignKey.ReferenceOption.Cascade
              )
          }
          .toMap

    lazy val childrenColumns : Set[Column]
      = children.view flatMap columnsForContainerTable toSet

    // lazy val deepNestedTableMappings : Iterable[TableMapping]
    //   = nestedTableMappings.view ++ 
    //     nestedTableMappings.view.map{_.deepNestedTableMappings}

    def columns : Set[Column]

    def primaryKeyColumns : Seq[Column]    

    def uniqueKeyColumns : Set[Seq[Column]]

    def indexColumns : Set[Seq[Column]]

    def foreignKeys : Map[Mapping, ForeignKey]

    lazy val table
      = Table(
          name
            = tableName,
          columns
            = columns.toList,
          primaryKey
            = primaryKeyColumns.map{_.name},
          uniqueKeys
            = uniqueKeyColumns.map{_.map{_.name}},
          indexes
            = indexColumns.map{_.map{_.name}},
          foreignKeys
            = foreignKeys.values.toSet
        )

  }