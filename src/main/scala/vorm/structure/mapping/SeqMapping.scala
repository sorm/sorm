package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

sealed class SeqMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends CollectionTableMapping
  { 
    lazy val item
      = Mapping( Membership.SeqItem(this), reflection.generics(0), settingsMap )

    lazy val primaryKeyColumns
      = ownerTable.get.primaryKeyColumns
          .map{ c => c.copy(name = "p_" + c.name, autoIncremented = false) } :+
        Column("i", Column.Type.SmallInt)

//
//    lazy val indexColumn
//      = MappedColumn.SeqIndex (
//          Column ("i", Column.Type.SmallInt),
//          this
//        )
//    lazy val parentKeyColumns
//      = ownerTable.flatMap {
//          _.primaryKeyColumns.view
//            .map(_.column)
//            .map(c => c copy (name = "p_" + c.name, autoIncremented = false))
//            .zipWithIndex
//            .map(MappedColumn.ParentKeyPart(_, _, this))
//        }
//    lazy val primaryKeyColumns
//      = parentKeyColumns :+ indexColumn
//
//    lazy val ownerTableColumns
//      = Nil

    // lazy val columns
    //   = primaryKeyColumns :++ item.ownerTableColumns
    // lazy val itemColumns
    //   = item match {
    //       case item : EntityMapping
    //         ⇒ item.primaryKeyColumns.view
    //             .map(_.column)
    //             .map(c => c copy (name = "v_"))

    //       case item : TableMapping
    //         ⇒ Nil
    //     }




    // lazy val primaryKeyColumns
    //   = ownerTable
    //       .map(_.primaryKeyColumns)
    //       .flatMap {
    //         _.map {
    //           c ⇒ c copy (
    //                 name = "p_" + c.name,
    //                 autoIncremented = false
    //               )
    //         }
    //       } :+
    //     Column("i", Column.Type.SmallInt)

    // lazy val columns
    //   = primaryKeyColumns :::
    //     item.

//    lazy val foreignKeys
//      = parentForeignKey ++: item.ownerTableForeignKeys
  }