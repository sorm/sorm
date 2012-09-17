package sorm.structure.mapping

import sorm._
import reflection._
import ddl._
import structure._
import sext.Sext._

import abstractSql._

trait TableMapping
  extends Mapping
  with HasChildren
  {
    def bindingsToContainerTable : Seq[(String, String)]
    lazy val abstractSqlTable : AbstractSql.Table
      = AbstractSql.Table(
          tableName,
          containerTableMapping.map{ c =>
            AbstractSql.Parent(
              c.abstractSqlTable,
              bindingsToContainerTable
            )
          }
        )
    lazy val abstractSqlPrimaryKeySelect : AbstractSql.Select
      = {
        val columns
          = primaryKeyColumns.view
              .map{_.name}
              .map{AbstractSql.Column(_, abstractSqlTable)}
              .toList
        AbstractSql.Select(
          expressions = columns,
          groupBy = columns
        )
      }
    lazy val abstractSqlResultSetSelect : AbstractSql.Select
      = AbstractSql.Select(
          for { (m, c) <- resultSetMappings }
          yield AbstractSql.Column(
                  c.name,
                  m.abstractSqlTable
                )  
        )
    private def deepTableMappings : Set[TableMapping]
      = nestedTableMappings.flatMap{_.deepTableMappings} + this

    //  todo: when all columns will be refactored to valuemappings to change it to just mappings
    lazy val resultSetMappings : Seq[(TableMapping, Column)]
      = deepTableMappings.toSeq.flatMap{ m => m.columns.map{m -> _} }


    /**
     * First descendant table mappings
     */
    lazy val nestedTableMappings : Set[TableMapping]
      = {
        def nestedTableMappings
          ( m : Mapping )
          : Iterable[TableMapping]
          = m match {
            case m : ColumnMapping ⇒ Nil
            case m : RangeMapping ⇒ Nil
            case m : TupleMapping ⇒ m.children.view flatMap nestedTableMappings
            case m : OptionMapping ⇒ m.children.view flatMap nestedTableMappings
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