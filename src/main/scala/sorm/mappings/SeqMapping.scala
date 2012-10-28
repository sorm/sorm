package sorm.mappings

import sext._, embrace._
import sorm._
import driver.DriverConnection
import core._
import jdbc.ResultSetView
import reflection.Reflection

class SeqMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings] )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.SeqItem(this), settings )
    lazy val index = new ValueMapping( Reflection[Int], Some(Membership.SeqIndex(this)), settings )
    lazy val primaryKeyColumns = masterTableColumns :+ index.column
    lazy val generatedColumns = primaryKeyColumns
    lazy val mappings = item +: Stream()

    def parseResultSet(rs: ResultSetView, c: DriverConnection)
      = rs.byNameRowsTraversable.view.map(item.valueFromContainerRow(_, c)).toVector

    override def update ( value : Any, masterKey : Stream[Any], connection : DriverConnection ) {
      connection.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey, connection)
    }

    override def insert ( v : Any, masterKey : Stream[Any], connection : DriverConnection ) {
      v.asInstanceOf[Seq[_]].view
        .zipWithIndex.foreach{ case (v, i) =>
          val pk = masterKey :+ i
          val values = item.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          connection.insert(tableName, values)
          item.insert(v, pk, connection)
        }
    }

    def valuesForContainerTableRow ( value : Any ) = Stream()
  }