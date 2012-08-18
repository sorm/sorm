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
    lazy val skeletonSelect
      = {
        import sql.Sql._

        def bindingsToContainer
          ( m : TableMapping )
          = m match {
              case m : CollectionMapping =>
                m.containerTableMappingForeignKey.get.bindings.view
              case m => 
                m.containerTableMapping.get.foreignKeys(m)
                  .bindings.view.map{_.swap}
            }

        val containerTableMappings
          = {
            def containerTableMappings
              ( m : Mapping )
              : Stream[TableMapping]
              = m.containerTableMapping
                  .map{ m => m +: containerTableMappings(m) }
                  .getOrElse(Stream())

            containerTableMappings(this).toIndexedSeq
          }

        val aliases
          = containerTableMappings.view
              .zipWithIndex
              .map{ case (v, i) => v -> (97 + i).toChar.toString }
              .toMap

        Select(
          what
            = containerTableMappings.last
                .primaryKeyColumns.toStream
                .map{_.name}
                .map{Column(_, aliases(containerTableMappings.last))},
          from
            = From( Table(containerTableMappings.last.tableName), 
                    Some( aliases(containerTableMappings.last) ) ),
          join
            = containerTableMappings.view
                .reverse
                .tail
                .map{ m =>
                  Join(
                    Table(m.tableName),
                    Some( aliases(m) ),
                    bindingsToContainer(m)
                      .map{ b =>
                        Column(b._1, Some(aliases(m))) →
                        Column(b._2, Some(aliases(m.containerTableMapping.get)))
                      }
                      .toList
                  )
                }

        )
      }




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

    def foreignKeys : Map[TableMapping, ForeignKey]

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