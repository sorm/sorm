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

      def fetchInstancesAndClose
        ( m : mapping.Table,
          indexes : Map[(mapping.Table, Column), Int] )
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
              rowsOfSubTables : Map[mapping.Table, Map[PrimaryKey, Row]] )

          type PrimaryKey = Seq[Any]

            
          def fetchRowsAndClose()
            : Map[PrimaryKey, Row]
            = {

              def updatedRows
                ( m : mapping.Table,
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
                case m : mapping.Value
                  ⇒ row.data(m.column)
                case m : mapping.Option
                  ⇒ Option( value( m.child.child, row ) )
                case m : mapping.Tuple
                  ⇒ m.reflection.instantiate(
                      m.children
                        .view
                        .map( _.child )
                        .map( value(_, row) )
                    )
                case m : mapping.Entity
                  ⇒ m.reflection.instantiate(
                      m.children
                        .view
                        .map( p ⇒ p.name → value(p.child, row) )
                        .toMap
                    )
                case m : mapping.Seq
                  ⇒ m.reflection.instantiate().asInstanceOf[Seq[_]] ++
                    row.rowsOfSubTables(m).values
                      .map( row ⇒ value( m.child.child, row) )
                case m : mapping.Set
                  ⇒ m.reflection.instantiate().asInstanceOf[Set[_]] ++
                    row.rowsOfSubTables(m).values
                      .map( row ⇒ value( m.child.child, row) )
                case m : mapping.Map
                  ⇒ m.reflection.instantiate().asInstanceOf[Map[_, _]] ++
                    row.rowsOfSubTables(m).values
                      .map( row ⇒ value( m.key.child, row ) → 
                                  value( m.value.child, row ) )
              }

          def values
            ( m : mapping.Table,
              rows : Map[PrimaryKey, Row] )
            = rows.values.map(value(m, _)).toList
          

          values( m, fetchRowsAndClose() )

        }

    }


}


