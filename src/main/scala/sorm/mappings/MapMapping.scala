package sorm.mappings

import sext.Sext._
import sorm._
import core._
import reflection.Reflection

class MapMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
  extends SlaveTableMapping
  {

    lazy val key = Mapping( reflection.generics(0), Membership.MapKey(this), settings, driver )
    lazy val value = Mapping( reflection.generics(1), Membership.MapValue(this), settings, driver )
    lazy val primaryKeyColumns = masterTableColumns :+ hashColumn
    lazy val hashColumn = ddl.Column( "h", ddl.ColumnType.Integer )
    lazy val mappings = key +: value +: Stream()
    def parseRows ( rows : Stream[String => Any] )
      = rows.map(r => key.valueFromContainerRow(r) -> value.valueFromContainerRow(r)).toMap


    override def update ( value : Any, masterKey : Stream[Any] ) {
      driver.delete(tableName, masterTableColumnNames zip masterKey)
      insert(value, masterKey)
    }

    override def insert ( v : Any, masterKey : Stream[Any] ) {
      v.asInstanceOf[Map[_, _]].view
        .zipWithIndex.foreach{ case ((k, v), i) =>
          val pk = masterKey :+ i
          val values = key.valuesForContainerTableRow(k) ++: value.valuesForContainerTableRow(v) ++: (primaryKeyColumnNames zip pk)
          driver.insert(tableName, values)
          key.insert(k, pk)
          value.insert(v, pk)
        }
    }

    def valuesForContainerTableRow ( v : Any ) = Stream()

  }