package vorm

import vorm._
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
              rowsOfSubmappings : RowGroupsOfMappings )

          type RowGroup = Map[Seq[Any], Row]

          type RowGroupsOfMappings = Map[Mapping, RowGroup]

            
          def fetchRowsAndClose()
            : RowGroupsOfMappings
            = {

              def value
                ( role : ColumnRole )
                = rs.value( indexesOfColumnRoles(role), role.jdbcType )

              def updatedRows
                ( mapping : Mapping,
                  rows : RowGroupsOfMappings )
                : RowGroupsOfMappings
                = ???



              var rows : RowGroupsOfMappings = Map()

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
            ( rows : RowGroupsOfMappings )
            : Map[Mapping, Seq[_]]
            = ???


          instances( fetchRowsAndClose() )

        }

    }


}


