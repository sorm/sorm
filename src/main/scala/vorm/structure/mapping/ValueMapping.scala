package vorm.structure.mapping

import vorm._
import extensions._
import reflection._
import ddl._
import select._
import structure._

sealed class ValueMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends Mapping
  {

    lazy val isKeyPart
      = ownerTable match {
        case Some(ownerTable)
          ⇒ ownerTable.primaryKey.contains(columnName) ||
            ownerTable.uniqueKeys.exists(_ contains columnName) ||
            ownerTable.indexes.exists(_ contains columnName)
      }

    lazy val columnType
      = reflection match {
          case Reflection[String]
            ⇒ if (isKeyPart)
                Column.Type.VarChar
              else
                Column.Type.Text
          case Reflection[Boolean]
            ⇒ Column.Type.Boolean
          case Reflection[Short]
            ⇒ Column.Type.SmallInt
          case Reflection[Int]
            ⇒ Column.Type.Integer
          case Reflection[Long]
            ⇒ Column.Type.BigInt
          case Reflection[Float]
            ⇒ Column.Type.Float
          case Reflection[Double]
            ⇒ Column.Type.Double
          case Reflection[BigDecimal]
            ⇒ Column.Type.Decimal
        }
    lazy val autoIncremented
      : Boolean
      = ???
    lazy val nullable
      = membership match {
          case Some(Membership.OptionItem(_)) ⇒ true
          case _ ⇒ false
        }
    lazy val column
      = Column(columnName, columnType, autoIncremented, nullable)

    lazy val ownerTableColumns
      = column :: Nil

    lazy val selectNodes 
      = Nil
  }