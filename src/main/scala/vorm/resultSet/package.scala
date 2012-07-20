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
        = fetchInstances( 
              Set() + columnRoles.head.mapping.root,
              columnRoles.view.zipWithIndex.toMap
            )
            .asInstanceOf[Seq[T]]

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

            
          def fetchRows()
            : RowGroupsOfMappings
            = {

              def value
                ( role : ColumnRole )
                // = rs.value( indexesOfColumnRoles(role), role.column.t.jdbcType )
                = role match {
                    case ColumnRole.Value( mapping )
                      ⇒ rs.value( indexesOfColumnRoles(role), mapping.column.jdbcType )
                    case ColumnRole.Hash( mapping )
                      ⇒ rs.value( indexesOfColumnRoles(role), Column.Type.Integer.jdbcType )
                  }

              def updatedRows
                ( mapping : Mapping,
                  rows : RowGroupsOfMappings )
                : RowGroupsOfMappings
                = throw new NotImplementedError



              var rows = RowGroupsOfMappings()

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
            = throw new NotImplementedError


          instances( fetchRows() )

        }

    }


}


