package sorm.mappings

import sorm._
import core._
import reflection._
import ddl._
import org.joda.time._

class ValueMapping
  ( val reflection : Reflection,
    val membership : Option[Membership],
    val settings : Map[Reflection, EntitySettings],
    val driver : Driver )
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
            ⇒ ColumnType.TinyInt
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

    def valueFromContainerRow ( data: String => Any ) = data(memberName)

    def valuesForContainerTableRow( value : Any ) = (memberName -> value) +: Stream()
  }