package sorm.mappings

import sext.Sext._
import sorm._
import core._
import jdbc.ResultSetView
import reflection._

class OptionMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.OptionItem(this), settings, driver )
    lazy val primaryKeyColumns = masterTableColumns
    lazy val generatedColumns = primaryKeyColumns
    lazy val mappings = item +: Stream()


    def parseResultSet(rs: ResultSetView)
      = rs.byNameRowsTraversable.view.headOption.map(item.valueFromContainerRow)

    override def update ( value : Any, masterKey : Stream[Any] ) {
      driver.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey)
    }

    override def insert ( v : Any, masterKey : Stream[Any] ) {
      v.asInstanceOf[Option[_]].view
        .zipWithIndex.foreach{ case (v, i) =>
          val pk = masterKey :+ i
          val values = item.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          driver.insert(tableName, values)
          item.insert(v, pk)
        }
    }

    def valuesForContainerTableRow ( value : Any ) = Stream()
  }
