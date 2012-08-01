package vorm.structure.mapping

import vorm._
import structure._
import reflection._
import extensions._
import vorm.{sql => Sql}

trait Table
  extends Mapping
  {
    lazy val primaryKeyColumns
      : scala.Seq[ddl.Column]
      = ???
    lazy val resultSetColumns
      : scala.Seq[ddl.Column]
      = ???
    lazy val allColumns
      : scala.Seq[ddl.Column]
      = ???
    lazy val valueMappings
      : scala.Seq[Mapping]
      = ???
    lazy val subTableMappings
      : scala.Seq[Table]
      = ???

    lazy val tableName
      : String
      = ???

    lazy val ownerTableColumnMappings
      : scala.Seq[(String, String)]
      = ???


    lazy val deepSubtableMappings
      : scala.Seq[Table]
      = subTableMappings.flatMap( _.deepSubtableMappings )

    lazy val skeletonSelectTableAliases
      : collection.immutable.Map[Mapping, String]
      = deepSubtableMappings.view
          .+:(this)
          .zipWithIndex
          .map{ case (m, i) ⇒ m → ("t" + i) }
          .toMap

    lazy val skeletonSelect
      : Sql.Select
      = {
        Sql.Select(
            what 
              = Vector(),
            from 
              = Sql.From(
                    Sql.Table( tableName ),
                    Some( skeletonSelectTableAliases(this) )
                  )
          )
          .foldFrom( deepSubtableMappings ) { (s, m) ⇒ 
            s.copy(
                join
                  = Sql.Join(
                        what  
                          = Sql.Table( m.tableName ),
                        as    
                          = Some( skeletonSelectTableAliases(m) ),
                        on    
                          = m.ownerTableColumnMappings
                              .map { case (l, r) ⇒ 
                                Sql.Column(l, Some(skeletonSelectTableAliases(m))) → 
                                Sql.Column(r, Some(skeletonSelectTableAliases(m.ownerTable)))
                              }
                      ) +:
                    s.join
              )
          }
      }

    lazy val resultSetSelect
      : ( Sql.Select, scala.Seq[(ddl.Column, Table)] )
      = skeletonSelect
          .foldFrom( this +: deepSubtableMappings ) { 
            ( s, m ) 
            ⇒ s.copy(
                  what 
                    = m.resultSetColumns.view
                        .map( _.name )
                        .map( sql.Column(_, Some(skeletonSelectTableAliases(m))) ) ++:
                      s.what
                )
          } → 
        resultSetColumns.map( _ → this )

    lazy val primaryKeySelect
      : Sql.Select
      = skeletonSelect.copy(
            what
              = primaryKeyColumns.view
                  .map( _.name )
                  .map( Sql.Column(_, Some(skeletonTablesAliases(this))) )
                  .toList
          )
  }