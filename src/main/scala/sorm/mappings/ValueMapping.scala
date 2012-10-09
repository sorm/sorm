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

    lazy val columnType : Column.Type
      = reflection match {
          case _ if reflection <:< Reflection.apply[String]
            ⇒ if (isKeyPart)
                Column.Type.VarChar
              else
                Column.Type.Text
          case _ if reflection <:< Reflection[BigDecimal]
            ⇒ Column.Type.Decimal
          case _ if reflection <:< Reflection[Boolean]
            ⇒ Column.Type.Boolean
          case _ if reflection <:< Reflection[Byte]
            ⇒ Column.Type.TinyInt
          case _ if reflection <:< Reflection[Short]
            ⇒ Column.Type.SmallInt
          case _ if reflection <:< Reflection[Int]
            ⇒ Column.Type.Integer
          case _ if reflection <:< Reflection[Long]
            ⇒ Column.Type.BigInt
          case _ if reflection <:< Reflection[Float]
            ⇒ Column.Type.Float
          case _ if reflection <:< Reflection[Double]
            ⇒ Column.Type.Double
          case _ if reflection <:< Reflection[DateTime]
            ⇒ Column.Type.TimeStamp
          case _ if reflection <:< Reflection[LocalTime]
            ⇒ Column.Type.Time
          case _ if reflection <:< Reflection[LocalDate]
            ⇒ Column.Type.Date
          case _
            ⇒ ???
        }

    def valueFromContainerRow(data: String => Any) = data(memberName)
  }