package sorm.mappings

import sext.Sext._
import sorm._
import core._
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
    lazy val mappings = item +: Stream()
    def parseRows ( rows : Stream[String => Any] )
      = rows.headOption.map(item.valueFromContainerRow)

    override def update ( value : Any, masterKey : Stream[Any] ) {
      driver.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey)
    }

    override def insert ( value : Any, masterKey : Stream[Any] ) {
      item match {
        case item : MasterTableMapping =>
          value.asInstanceOf[Option[_]].foreach{ v =>
            val values = (primaryKeyColumnNames zip masterKey) ++: item.valuesForContainerTableRow(v)
            driver.insert(tableName, values)
          }
        case item =>
          value.asInstanceOf[Option[_]].foreach{ v =>
            val pk = masterKey
            driver.insert(tableName, primaryKeyColumnNames zip pk)
            item.insert(v, pk)
          }
      }
    }

    def valuesForContainerTableRow ( value : Any ) = Stream()
  }
