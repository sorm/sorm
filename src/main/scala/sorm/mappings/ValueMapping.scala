package sorm.mappings

import sorm._
import driver.DriverConnection
import core._
import reflection._
import ddl._
import org.joda.time._

class ValueMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings] )
  extends ColumnMapping {

    lazy val columnType : ColumnType
      = reflection match {
          case _ if reflection <:< Reflection.apply[String]
            ⇒ if (isKeyPart)
                ColumnType.VarChar
              else
                ColumnType.Text
          case _ if reflection <:< Reflection[BigDecimal]
            ⇒ ColumnType.Decimal
          case _ if reflection <:< Reflection[Boolean]
            ⇒ ColumnType.Boolean
          case _ if reflection <:< Reflection[Byte]
            ⇒ ColumnType.SmallInt //  postgres dissuport of tinyint workaround
          case _ if reflection <:< Reflection[Short]
            ⇒ ColumnType.SmallInt
          case _ if reflection <:< Reflection[Int]
            ⇒ ColumnType.Integer
          case _ if reflection <:< Reflection[Long]
            ⇒ ColumnType.BigInt
          case _ if reflection <:< Reflection[Float]
            ⇒ ColumnType.Float
          case _ if reflection <:< Reflection[Double]
            ⇒ ColumnType.Double
          case _ if reflection <:< Reflection[DateTime]
            ⇒ ColumnType.TimeStamp
          case _ if reflection <:< Reflection[LocalTime]
            ⇒ ColumnType.Time
          case _ if reflection <:< Reflection[LocalDate]
            ⇒ ColumnType.Date
          case _
            ⇒ ???
        }

    private def isKeyPart
      = {
        def isKeyPart
          ( m : Mapping )
          : Boolean
          = m.membership
              .map{
                case Membership.EntityId(_) =>
                  true
                case Membership.EntityProperty(n, e) =>
                  val s = e.settings(e.reflection)
                  s.uniqueKeys.view.flatten.exists(_ == n) ||
                  s.indexes.view.flatten.exists(_ == n)
                case Membership.TupleItem(_, m) =>
                  isKeyPart(m)
                case Membership.OptionToNullableItem(m) =>
                  isKeyPart(m)
                case _ =>
                  false
              }
              .getOrElse(false)

        isKeyPart(this)
      }


    def valueFromContainerRow ( data: String => Any, c : DriverConnection ) = data(memberName)

    def valuesForContainerTableRow( value : Any ) = (memberName -> value) +: Stream()
  }