package sorm.mappings

import sext._, embrace._

import sorm._
import driver.DriverConnection
import core._
import reflection._

class OptionToNullableMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings] )
  extends CompositeMapping
  {
    type T = Option[_]

    lazy val item = Mapping( reflection.generics(0), Membership.OptionToNullableItem(this), settings )
    lazy val mappings = item +: Stream()

    def valueFromContainerRow ( row : String => Any, connection : DriverConnection )
      = if( columnsForContainer.map(_.name).forall(row(_) == null) ) None
        else item.valueFromContainerRow(row, connection) $ (Some(_))

    def valuesForContainerTableRow ( value : Any )
      = value match {
          case Some(value) => item.valuesForContainerTableRow(value)
          case None => columnsForContainer.map(_.name -> null)
        }

    override def update ( value : Any, masterKey : Stream[Any], connection : DriverConnection ) {
      value.asInstanceOf[T] foreach (item.update(_, masterKey, connection))
    }

    override def insert ( value : Any, masterKey : Stream[Any], connection : DriverConnection ) {
      value.asInstanceOf[T] foreach (item.insert(_, masterKey, connection))
    }


  }
