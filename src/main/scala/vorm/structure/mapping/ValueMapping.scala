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
      = ownerTable.get.primaryKeyColumns.exists{_.name == columnName} ||
        ???
//          ⇒ ownerTable.primaryKey.contains(columnName) ||
//            ownerTable.uniqueKeys.exists(_ contains columnName) ||
//            ownerTable.indexes.exists(_ contains columnName)

    lazy val columnType
      = reflection match {
          case _ if reflection inheritsFrom Reflection.apply[String]
            ⇒ if (isKeyPart)
                Column.Type.VarChar
              else
                Column.Type.Text
          case _ if reflection inheritsFrom Reflection[Boolean]
            ⇒ Column.Type.Boolean
          case _ if reflection inheritsFrom Reflection[Short]
            ⇒ Column.Type.SmallInt
          case _ if reflection inheritsFrom Reflection[Int]
            ⇒ Column.Type.Integer
          case _ if reflection inheritsFrom Reflection[Long]
            ⇒ Column.Type.BigInt
          case _ if reflection inheritsFrom Reflection[Float]
            ⇒ Column.Type.Float
          case _ if reflection inheritsFrom Reflection[Double]
            ⇒ Column.Type.Double
          case _ if reflection inheritsFrom Reflection[BigDecimal]
            ⇒ Column.Type.Decimal
        }
    lazy val autoIncremented
      : Boolean
      = {
        def autoIncremented
          ( m : Mapping )
          : Boolean
          = m.membership
              .map{
                case Membership.EntityProperty(name, entity) 
                  ⇒ entity.settings.autoIncrement(name)
                case Membership.TupleItem(_, tuple)
                  ⇒ autoIncremented(tuple)
                case Membership.OptionItem(option)
                  ⇒ autoIncremented(option)
                case _
                  ⇒ false
              }
              .getOrElse( false )

        autoIncremented( this )
      }
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