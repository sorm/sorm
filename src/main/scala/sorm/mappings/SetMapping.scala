package sorm.mappings

import sext._
import sorm._
import connection.Connection
import core._
import jdbc.ResultSetView
import reflection.Reflection

class SetMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings] )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.SetItem(this), settings )
    lazy val primaryKeyColumns = masterTableColumns :+ hashColumn
    lazy val generatedColumns = primaryKeyColumns
    lazy val hashColumn = ddl.Column( "h", ddl.ColumnType.Integer )
    lazy val mappings = item +: Stream()

    def parseResultSet(rs: ResultSetView, c: Connection)
      = rs.byNameRowsTraversable.view.map(item.valueFromContainerRow(_, c)).toSet

    override def update ( value : Any, masterKey : Stream[Any], connection : Connection ) {
      connection.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey, connection)
    }

    override def insert ( v : Any, masterKey : Stream[Any], connection : Connection ) {
      v.asInstanceOf[Set[_]].view
        .zipWithIndex.foreach{ case (v, i) =>
          val pk = masterKey :+ i
          val values = item.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          connection.insert(tableName, values)
          item.insert(v, pk, connection)
        }
    }

    def valuesForContainerTableRow ( value : Any ) = Stream()

  }
