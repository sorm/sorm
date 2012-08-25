package sorm.ddl

import sorm._
import extensions.Extensions._

sealed case class Table
  ( name : String,
    columns : Seq[Column],
    primaryKey : Seq[String],
    uniqueKeys : Set[Seq[String]] = Set.empty,
    indexes : Set[Seq[String]] = Set.empty,
    foreignKeys : Set[ForeignKey] = Set.empty )
  {
    def ddl
      = {
        def primaryKeyDdl
          = "PRIMARY KEY (" + 
            primaryKey.view.map{quote}.mkString(", ") + 
            ")"
        def indexDdl
          ( key : Seq[String] )
          = "KEY (" + key.view.map{quote}.mkString(", ") + ")"
        def uniqueKeyDdl
          ( key : Seq[String] )
          = "UNIQUE (" + key.view.map{quote}.mkString(", ") + ")"
            "CREATE TABLE " + quote(name) + "\n" +
            ( "( " + 
              ( ( columns.view.map{_.ddl} ++
                  primaryKeyDdl.some ++
                  indexes.view.map{indexDdl} ++
                  uniqueKeys.view.map{uniqueKeyDdl} ++
                  foreignKeys.view.map{_.ddl}
                  )
                  .mkString(",\n") +
                  " )"
                )
                .indent(2).trim
              )
              .indent(2)
      }
  }