package sorm.mappings

import sext._
import sorm._
import core._
import jdbc.ResultSetView
import reflection.Reflection

class SetMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.SetItem(this), settings, driver )
    lazy val primaryKeyColumns = masterTableColumns :+ hashColumn
    lazy val generatedColumns = primaryKeyColumns
    lazy val hashColumn = ddl.Column( "h", ddl.ColumnType.Integer )
    lazy val mappings = item +: Stream()

    def parseResultSet(rs: ResultSetView)
      = rs.byNameRowsTraversable.view.map(item.valueFromContainerRow).toSet

    override def update ( value : Any, masterKey : Stream[Any] ) {
      driver.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey)
    }

    override def insert ( v : Any, masterKey : Stream[Any] ) {
      v.asInstanceOf[Set[_]].view
        .zipWithIndex.foreach{ case (v, i) =>
          val pk = masterKey :+ i
          val values = item.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          driver.insert(tableName, values)
          item.insert(v, pk)
        }
    }

    def valuesForContainerTableRow ( value : Any ) = Stream()

  }
