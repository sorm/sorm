package sorm.mappings

import sext._, embrace._
import sorm._
import driver.DriverConnection
import core._
import jdbc.ResultSetView
import reflection.Reflection

class MapMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings] )
  extends SlaveTableMapping
  {

    lazy val key = Mapping( reflection.generics(0), Membership.MapKey(this), settings )
    lazy val value = Mapping( reflection.generics(1), Membership.MapValue(this), settings )
    lazy val primaryKeyColumns = masterTableColumns :+ hashColumn
    lazy val generatedColumns = primaryKeyColumns
    lazy val hashColumn = ddl.Column( "h", ddl.ColumnType.Integer )
    lazy val mappings = key +: value +: Stream()
    def parseResultSet(rs: ResultSetView, c : DriverConnection)
      = rs.byNameRowsTraversable.view.map(r => key.valueFromContainerRow(r, c) -> value.valueFromContainerRow(r, c)).toMap


    override def update ( value : Any, masterKey : Stream[Any], connection : DriverConnection ) {
      connection.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey, connection)
    }

    override def insert ( v : Any, masterKey : Stream[Any], connection : DriverConnection ) {
      v.asInstanceOf[Map[_, _]].view
        .zipWithIndex.foreach{ case ((k, v), i) =>
          val pk = masterKey :+ i
          val values = key.valuesForContainerTableRow(k) ++: value.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          connection.insert(tableName, values)
          key.insert(k, pk, connection)
          value.insert(v, pk, connection)
        }
    }

    def valuesForContainerTableRow ( v : Any ) = Stream()

  }