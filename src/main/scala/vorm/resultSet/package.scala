package vorm

import vorm._
import extensions._
import reflection._
import structure._
import jdbc._
import ddl._
import java.sql.ResultSet

package object resultSet {

  implicit class ResultSetExtensions
    ( rs : ResultSet )
    {
      def fetchInstances
        [ T ]
        ( columnRoles : IndexedSeq[ColumnRole] )
        : Seq[T]
        = ???
        // = fetchInstances( 
        //       Set() + columnRoles.head.mapping.root,
        //       columnRoles.view.zipWithIndex.toMap
        //     )
        //     .apply(columnRoles.head.mapping.root)
        //     .asInstanceOf[Seq[T]]

      def fetchInstances
        ( mappings : Set[Mapping],
          indexesOfColumnRoles : Map[ColumnRole, Int] )
        : Map[Mapping, Seq[_]]
        = {

          case class Row
            ( data : Map[ColumnRole, Any],
              rowsOfSubmappings : Map[Mapping, Map[PrimaryKey, Row]] )

          type PrimaryKey = Seq[Any]
            
          def fetchRowsAndClose()
            : Map[Mapping, Map[PrimaryKey, Row]]
            = {

              def value
                ( role : ColumnRole )
                = rs.value( indexesOfColumnRoles(role), role.jdbcType )

              def updatedRows
                ( mapping : Mapping,
                  rows : Map[PrimaryKey, Row] )
                : Map[PrimaryKey, Row]
                = mapping match {
                    case mapping : structure.mapping.Entity
                      ⇒ val primaryKey = mapping.primaryKeyColumnRoles map value
                        rows get primaryKey match {
                          case Some(row)
                            ⇒ rows +
                              ( primaryKey,
                                row copy (
                                  rowsOfSubmappings
                                    = rowsOfSubmappings
                                        .map {
                                          case (m, rows)
                                            ⇒ updatedRows(m, rows)
                                        }
                                  )
                                )
                          case None
                            ⇒ rows + 
                              ( primaryKey,
                                Row(
                                  data 
                                    = mapping.resultSetColumnRoles.view
                                        .zipBy(value).toMap,
                                  rowsOfSubmappings
                                    = mapping.subTableMappings.view
                                        .zipBy(updatedRows(_, Map())).toMap
                                  )
                                )
                        }
                  }



              var rows : Map[Mapping, Map[PrimaryKey, Row]] = Map()

              rs.beforeFirst()
              while ( rs.next() ) {
                for ( mapping ← mappings ) {
                  rows = updatedRows( mapping, rows )
                }
              }
              rs.close()

              rows
            }


          def instances
            ( rows : Map[Mapping, Map[PrimaryKey, Row]] )
            : Map[Mapping, Seq[_]]
            = ???


          instances( fetchRowsAndClose() )

        }

    }


}


