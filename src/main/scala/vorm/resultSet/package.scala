package vorm

import vorm._
import reflection._
import structure._
import mapping._
import jdbc._
import extensions._
import ddl._
import java.sql.ResultSet

package object resultSet {

  implicit class ResultSetExtensions
    ( rs : ResultSet )
    extends LogulaLogging
    {

      def fetchInstancesAndClose
        ( m : TableMapping,
          indexes : Map[(TableMapping, Column), Int] )
        : Seq[_]
        = {
          //  ideas
          // type PrimaryKey 
          //   = Seq[Any]
          // type Rows 
          //   = Map[PrimaryKey, Row]
          // type Row 
          //   = Map[String, Either[Any, Rows]]
          // class Rows 
          //   extends collection.immutable.HashMap[PrimaryKey, Map[String, Either[Any, Rows]]]

          case class Row
            ( data : Map[Column, Any],
              rowsOfSubTables : Map[TableMapping, Map[PrimaryKey, Row]] )

          type PrimaryKey = Seq[Any]

            
          def fetchRowsAndClose()
            : Map[PrimaryKey, Row]
            = {

              def updatedRows
                ( m : TableMapping,
                  rows : Map[PrimaryKey, Row] )
                : Map[PrimaryKey, Row]
                = {
                  def value
                    ( column : Column )
                    = rs.value(
                        indexes(m → column) + 1,
                        column.t.jdbcType
                      )

                  val primaryKey
                    = m.primaryKeyColumns map value

                  rows.get(primaryKey) match {
                    case Some(row)
                      ⇒ rows +
                        ( primaryKey → 
                          row.copy(
                              rowsOfSubTables
                                = row.rowsOfSubTables.map {
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
                              = m.childrenColumns
                                  .view
                                  .zipBy(value)
                                  .toMap,
                            rowsOfSubTables
                              = m.nestedTableMappings
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
                rows = updatedRows( m, rows )
              }
              rs.close()

              rows
            }


          def value
            ( m : Mapping,
              row : Row )
            : Any
            = m match {
                case m : ValueMapping =>
                  row.data( m.column )
                case m : OptionMapping =>
                  Option( value( m.item, row ) )
                case m : TupleMapping =>
                  m.reflection.instantiate(
                    m.items.map{ value( _, row ) }
                  )
                case m : EntityMapping =>
                  m.reflection.instantiate(
                    m.properties.mapValues{
                      case p : EntityMapping =>
                        values(p, row.rowsOfSubTables(p)).head
                      case p =>
                        value( p, row )
                    }
                  )
                case m : SeqMapping =>
                  row.rowsOfSubTables(m).values
                    .map{ value( m.item, _ ) }
                    .toIndexedSeq
                case m : SetMapping =>
                  row.rowsOfSubTables(m).values
                    .map{ value( m.item, _ ) }
                    .toSet
                case m : MapMapping =>
                  row.rowsOfSubTables(m).values
                    .map{ row ⇒ value( m.key, row ) ->
                                value( m.value, row ) }
                    .toMap
              }

          def values
            ( m : TableMapping,
              rows : Map[PrimaryKey, Row] )
            = rows.values.map{ value(m, _) }.toList
          

          values( m, fetchRowsAndClose() )

        }

    }


}


