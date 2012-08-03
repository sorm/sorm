package vorm

import vorm._
import reflection._
import structure._
import mapping._
import jdbc._
import extensions._
import java.sql.ResultSet

package object resultSet {

  implicit class ResultSetExtensions
    ( rs : ResultSet )
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
                          indexes(m → column),
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
                              = m.resultSetColumns
                                  .view
                                  .zipBy(value)
                                  .toMap,
                            rowsOfSubTables
                              = m.subTableMappings
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
                case m : ValueMapping
                  ⇒ row.data(m.column)
                case m : OptionMapping
                  ⇒ Option( value( m.child.child, row ) )
                case m : TupleMapping
                  ⇒ m.reflection.instantiate(
                      m.children
                        .view
                        .map( _.child )
                        .map( value(_, row) )
                    )
                case m : EntityMapping
                  ⇒ m.reflection.instantiate(
                      m.children
                        .view
                        .map( p ⇒ p.name → value(p.child, row) )
                        .toMap
                    )
                case m : SeqMapping
                  ⇒ m.reflection.instantiate().asInstanceOf[Seq[_]] ++
                    row.rowsOfSubTables(m).values
                      .map( row ⇒ value( m.child.child, row) )
                case m : SetMapping
                  ⇒ m.reflection.instantiate().asInstanceOf[Set[_]] ++
                    row.rowsOfSubTables(m).values
                      .map( row ⇒ value( m.child.child, row) )
                case m : MapMapping
                  ⇒ m.reflection.instantiate().asInstanceOf[Map[_, _]] ++
                    row.rowsOfSubTables(m).values
                      .map( row ⇒ value( m.key.child, row ) → 
                                  value( m.value.child, row ) )
              }

          def values
            ( m : TableMapping,
              rows : Map[PrimaryKey, Row] )
            = rows.values.map(value(m, _)).toList
          

          values( m, fetchRowsAndClose() )

        }

    }


}


