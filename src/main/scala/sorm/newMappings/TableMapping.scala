package sorm.newMappings

import sext.Sext._

import sorm._
import reflection.Reflection
import core._
import scala.Some
import ddl._

trait TableMapping extends CompositeMapping with Querying {

  def primaryKey : Stream[String]

  lazy val uniqueKeys : Set[Seq[String]]
    = settings get reflection map (_.uniqueKeys) getOrElse Set()
  lazy val indexes : Set[Seq[String]]
    = settings get reflection map (_.indexes) getOrElse Set()

  def isMasterTable : Boolean

  lazy val masterTableForeignKey : Option[ForeignKey]
    = containerTableMapping
        .flatMap(_.satisfying(_.isMasterTable))
        .map(m => ForeignKey(
          m.tableName,
          m.primaryKey.map(n => "p$" + n -> n),
          ForeignKey.ReferenceOption.Cascade
        ))

  lazy val foreignKeyForContainer : Option[ForeignKey]
    = if( isMasterTable )
        Some(ForeignKey(
          tableName,
          primaryKey.map(n => memberName + "$" + n -> n),
          ForeignKey.ReferenceOption.Cascade
        ))
      else
        None

  private lazy val containedForeignKeys : Stream[ForeignKey]
    = containedTableMappings flatMap (_.foreignKeyForContainer)
  // lazy val foreignKeyByTable : Map[TableMapping, ForeignKey]
  //   = containedTableMappings
  //       .filter(_.isMasterTable)
  //       .map()

  lazy val tableName : String
    = if (isMasterTable)
        ddlName(reflection.name)
      else
        containerTableMapping.map(_.tableName + "$").getOrElse("") + memberName

  lazy val table
    = Table(
        name
          = tableName,
        columns
          = columns,
        primaryKey
          = primaryKey,
        uniqueKeys
          = uniqueKeys,
        indexes
          = indexes,
        foreignKeys
          = masterTableForeignKey ++: containedForeignKeys ++: Set()
      )
}
