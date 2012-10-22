package sorm.mappings

import sext._
import sorm._
import connection.Connection
import core._
import jdbc.ResultSetView
import reflection.Reflection

class MapMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends SlaveTableMapping
  {

    lazy val key = Mapping( reflection.generics(0), Membership.MapKey(this), settings, connection )
    lazy val value = Mapping( reflection.generics(1), Membership.MapValue(this), settings, connection )
    lazy val primaryKeyColumns = masterTableColumns :+ hashColumn
    lazy val generatedColumns = primaryKeyColumns
    lazy val hashColumn = ddl.Column( "h", ddl.ColumnType.Integer )
    lazy val mappings = key +: value +: Stream()
    def parseResultSet(rs: ResultSetView)
      = rs.byNameRowsTraversable.view.map(r => key.valueFromContainerRow(r) -> value.valueFromContainerRow(r)).toMap


    override def update ( value : Any, masterKey : Stream[Any] ) {
      connection.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey)
    }

    override def insert ( v : Any, masterKey : Stream[Any] ) {
      v.asInstanceOf[Map[_, _]].view
        .zipWithIndex.foreach{ case ((k, v), i) =>
          val pk = masterKey :+ i
          val values = key.valuesForContainerTableRow(k) ++: value.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          connection.insert(tableName, values)
          key.insert(k, pk)
          value.insert(v, pk)
        }
    }

    def valuesForContainerTableRow ( v : Any ) = Stream()

  }