package sorm.mappings

import sext.Sext._
import sorm._
import core._
import jdbc.ResultSetView
import reflection.Reflection

class SeqMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends SlaveTableMapping
  {

    lazy val item = Mapping( reflection.generics(0), Membership.SeqItem(this), settings, driver )
    lazy val index = new ValueMapping( Reflection[Int], Some(Membership.SeqIndex(this)), settings, driver )
    lazy val primaryKeyColumns = masterTableColumns :+ index.column
    lazy val generatedColumns = primaryKeyColumns
    lazy val mappings = item +: Stream()
    def parseResultSet(rs: ResultSetView)
      = rs.byNameRowsTraversable.view.map(item.valueFromContainerRow).toVector


    override def update ( value : Any, masterKey : Stream[Any] ) {
      driver.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey)
    }

    override def insert ( value : Any, masterKey : Stream[Any] ) {
      item match {
        case item : MasterTableMapping =>
          value.asInstanceOf[Seq[_]].view
            .zipWithIndex.foreach{ case (v, i) =>
              val values = (primaryKeyColumnNames zip (masterKey :+ i)) ++: item.valuesForContainerTableRow(v)
              driver.insert(tableName, values)
            }
        case item =>
          value.asInstanceOf[Seq[_]].view
            .zipWithIndex.foreach{ case (v, i) =>
              val pk = masterKey :+ i
              driver.insert(tableName, primaryKeyColumnNames zip pk)
              item.insert(v, pk)
            }
      }
    }

    def valuesForContainerTableRow ( value : Any ) = Stream()
  }