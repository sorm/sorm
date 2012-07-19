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
      def results
        [ T : TypeTag ]
        ( columnRoles : IndexedSeq[ColumnRole] )
        : Seq[T]
        = {
          def value
            ( index : Int )
            = columnRoles(index) match {
                case ColumnRole.Value( mapping )
                  ⇒ rs.value( index, mapping.column.jdbcType )
                case ColumnRole.Hash( mapping )
                  ⇒ rs.value( index, Column.Type.Integer.jdbcType )
              }

          val columnRolesWithIndexesStream
            = columnRoles.toStream.zipWithIndex

          val rowData
            : Stream[(Any, ColumnRole)]
            = columnRolesWithIndexesStream
                .map { case (role, i) ⇒ rs.getValue(i) → role }

          
        }

    }
}


