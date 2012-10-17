package sorm.mappings

import sext._
import sorm._
import core._
import jdbc.ResultSetView
import reflection._

class OptionToTableMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.OptionToTableItem(this), settings, driver )
    lazy val primaryKeyColumns = masterTableColumns
    lazy val generatedColumns = primaryKeyColumns
    lazy val mappings = item +: Stream()


    def parseResultSet(rs: ResultSetView)
      = rs.byNameRowsTraversable.toStream.headOption.map(item.valueFromContainerRow)

    override def update ( value : Any, masterKey : Stream[Any] ) {
      driver.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey)
    }

    override def insert ( v : Any, masterKey : Stream[Any] ) {
      v.asInstanceOf[Option[_]]
        .foreach{ v =>
          val pk = masterKey
          val values = item.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          driver.insert(tableName, values)
          item.insert(v, pk)
        }
    }

    def valuesForContainerTableRow ( value : Any ) = Stream()
  }
