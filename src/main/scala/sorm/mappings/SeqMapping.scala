package sorm.mappings

import sext._
import sorm._
import connection.Connection
import core._
import jdbc.ResultSetView
import reflection.Reflection

class SeqMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val connection : Connection )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.SeqItem(this), settings, connection )
    lazy val index = new ValueMapping( Reflection[Int], Some(Membership.SeqIndex(this)), settings, connection )
    lazy val primaryKeyColumns = masterTableColumns :+ index.column
    lazy val generatedColumns = primaryKeyColumns
    lazy val mappings = item +: Stream()

    def parseResultSet(rs: ResultSetView)
      = rs.byNameRowsTraversable.view.map(item.valueFromContainerRow).toVector

    override def update ( value : Any, masterKey : Stream[Any] ) {
      connection.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey)
    }

    override def insert ( v : Any, masterKey : Stream[Any] ) {
      v.asInstanceOf[Seq[_]].view
        .zipWithIndex.foreach{ case (v, i) =>
          val pk = masterKey :+ i
          val values = item.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          connection.insert(tableName, values)
          item.insert(v, pk)
        }
    }

    def valuesForContainerTableRow ( value : Any ) = Stream()
  }