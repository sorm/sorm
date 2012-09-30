package sorm.mappings

import sext.Sext._

import sorm._
import reflection.Reflection
import core._
import ddl._

sealed trait TableMapping extends CompositeMapping with Querying {

  def name : String
  def foreignKeys : Set[ForeignKey]
  def primaryKeyColumns : Stream[Column]

  lazy val uniqueKeys : Set[Seq[String]]
    = settings get reflection map (_.uniqueKeys) getOrElse Set()

  lazy val indexes : Set[Seq[String]]
    = settings get reflection map (_.indexes) getOrElse Set()

  lazy val containedForeignKeys : Stream[ForeignKey]
    = containedTableMappings collect { case m : MasterTableMapping => m.foreignKeyForContainer }

  lazy val table = Table(name, primaryKeyColumns ++: columns, primaryKey, uniqueKeys, indexes, foreignKeys)

  lazy val primaryKey = primaryKeyColumns.map(_.name)

  override def valueFromContainerRow ( row : String => Any ) : Any
    = containerTableMapping.get.primaryKey.zipBy(row).toMap as fetchByContainerPrimaryKey

}

trait MasterTableMapping extends TableMapping {
  lazy val name = ddlName(reflection.name)
  lazy val foreignKeyForContainer : ForeignKey
    = ForeignKey(
        name,
        primaryKey.map(n => memberName + "$" + n -> n),
        ForeignKey.ReferenceOption.Cascade
      )
  lazy val columnsForContainer : Stream[Column]
    = primaryKeyColumns.map(c => c.copy(
        autoIncrement = false,
        name = memberName + "$" + c.name
      ))
  lazy val bindingsToContainerTable
    = foreignKeyForContainer.bindings.toStream map (_.swap)
  lazy val foreignKeys = Set() ++ containedForeignKeys
}

trait SlaveTableMapping extends TableMapping {
  lazy val name = masterTableMapping.name + "$" + memberName
  lazy val masterTableMapping = containerTableMapping.get
  lazy val masterTableForeignKey 
    = ForeignKey(
        masterTableMapping.name,
        masterTableMapping.primaryKey.map(n => "p$" + n -> n),
        ForeignKey.ReferenceOption.Cascade
      )
  lazy val bindingsToContainerTable = masterTableForeignKey.bindings.toStream
  lazy val foreignKeys = Set() ++ containedForeignKeys + masterTableForeignKey
  lazy val masterTableColumns = masterTableMapping.primaryKeyColumns.map(c => c.copy(name = "p$" + c.name, autoIncrement = false))

}

