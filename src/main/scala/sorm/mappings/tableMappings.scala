package sorm.mappings

import sext.Sext._

import sorm._
import reflection.Reflection
import core._
import ddl._

sealed trait TableMapping extends CompositeMapping with Querying {

  def tableName : String
  def foreignKeys : Set[ForeignKey]
  def primaryKeyColumns : Stream[Column]
  def generatedColumns : Stream[Column]
  override lazy val tableColumns = generatedColumns ++: mappings.flatMap(_.columnsForContainer)

  def uniqueKeys : Set[Seq[String]] = Set()
  def indexes : Set[Seq[String]] = Set()

  lazy val containedForeignKeys : Stream[ForeignKey]
    = containedTableMappings collect { case m : MasterTableMapping => m.foreignKeyForContainer }

  lazy val table = Table(tableName, primaryKeyColumns ++: tableColumns distinct, primaryKeyColumnNames, uniqueKeys, indexes, foreignKeys)

  lazy val primaryKeyColumnNames = primaryKeyColumns.map(_.name)

  override def valueFromContainerRow ( data : String => Any ) : Any
    = containerTableMapping.get.primaryKeyColumnNames.zipBy(data).toMap as fetchByContainerPrimaryKey

}

trait MasterTableMapping extends TableMapping {
  lazy val tableName = ddlName(reflection.name)
  lazy val foreignKeyForContainer : ForeignKey
    = ForeignKey(
        tableName,
        primaryKeyColumnNames.map(n => memberName + "$" + n -> n),
        ReferenceOption.Cascade
      )
  override lazy val columnsForContainer : Stream[Column]
    = primaryKeyColumns.map(c => c.copy(
        autoIncrement = false,
        name = memberName + "$" + c.name
      ))
  lazy val bindingsToContainerTable
    = foreignKeyForContainer.bindings.toStream map (_.swap)
  lazy val foreignKeys = Set() ++ containedForeignKeys

}

trait SlaveTableMapping extends TableMapping {
  lazy val tableName = masterTableMapping.tableName + "$" + memberName
  lazy val masterTableMapping = containerTableMapping.get
  lazy val masterTableForeignKey 
    = ForeignKey(
        masterTableMapping.tableName,
        masterTableMapping.primaryKeyColumnNames.map(n => "p$" + n -> n),
        ReferenceOption.Cascade
      )
  lazy val bindingsToContainerTable = masterTableForeignKey.bindings.toStream
  lazy val foreignKeys = Set() ++ containedForeignKeys + masterTableForeignKey
  lazy val masterTableColumns = masterTableMapping.primaryKeyColumns.map(c => c.copy(name = "p$" + c.name, autoIncrement = false))
  lazy val masterTableColumnNames = masterTableColumns.map(_.name)
  override def columnsForContainer = Stream()
}

