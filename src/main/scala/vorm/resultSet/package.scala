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
      type MappingColumn = (structure.mapping.Table, Column)

      def fetchInstances
        ( mapping : structure.mapping.Table,
          indexes : Map[MappingColumn, Int] )
        : Seq[_]
        = {

          case class Row
            ( data : Map[Column, Any],
              rowsOfSubmappings : Map[structure.mapping.Table, Map[PrimaryKey, Row]] )
          type PrimaryKey = Seq[Any]
            
          def fetchRowsAndClose()
            : Map[PrimaryKey, Row]
            = {

              def updatedRows
                ( mapping : structure.mapping.Table,
                  rows : Map[PrimaryKey, Row] )
                : Map[PrimaryKey, Row]
                = {
                  def value
                    ( column : Column )
                    = rs.value(
                          indexes(mapping → column),
                          column.t.jdbcType
                        )

                  val primaryKey
                    = mapping.primaryKeyColumns map value

                  rows.get(primaryKey) match {
                    case Some(row)
                      ⇒ rows +
                        ( primaryKey → 
                          row.copy(
                              rowsOfSubmappings
                                = row.rowsOfSubmappings.map {
                                    case (m, rows)
                                      ⇒ m → updatedRows(m, rows)
                                  }
                            )
                        )
                    case None
                      ⇒ rows +
                        ( primaryKey → 
                          Row(
                              data
                                = mapping.resultSetColumns
                                    .view
                                    .zipBy(value)
                                    .toMap,
                              rowsOfSubmappings
                                = mapping.subTableMappings
                                    .view
                                    .zipBy( updatedRows(_, Map()) )
                                    .toMap
                            )
                        )
                  }
                  

                }


              var rows : Map[PrimaryKey, Row] = Map()

              rs.beforeFirst()
              while ( rs.next() ) {
                rows = updatedRows( mapping, rows )
              }
              rs.close()

              rows
            }


          def instances
            ( rows : Map[PrimaryKey, Row] )
            : Seq[_]
            = ???


          instances( fetchRowsAndClose() )

        }

    }


}


