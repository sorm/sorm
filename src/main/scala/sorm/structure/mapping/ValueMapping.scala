package sorm.structure.mapping

import sorm._
import extensions.Extensions._
import reflection._
import ddl._
import structure._

sealed class ValueMapping
  ( val membership : Option[Membership],
    val reflection : Reflection,
    settingsMap : SettingsMap )
  extends Mapping
  {

    lazy val isKeyPart
      = {
        def isKeyPart
          ( m : Mapping )
          : Boolean
          = m.membership
              .map{
                case Membership.EntityId(_) =>
                  true
                case Membership.EntityProperty(n, e) =>
                  e.settings.uniqueKeys.view.flatten.exists(_ == n) ||
                  e.settings.indexes.view.flatten.exists(_ == n)
                case Membership.TupleItem(_, m) =>
                  isKeyPart(m)
                case Membership.OptionItem(m) =>
                  isKeyPart(m)
                case _ =>
                  false
              }
              .getOrElse(false)

        isKeyPart(this)
      }

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
          case _
            ⇒ ???
        }

    lazy val autoIncremented
      = membership match {
          case Some(Membership.EntityId(_)) => true
          case _ => false
        }

    lazy val nullable
      = {
        def ancestorsToTable
          ( m : Mapping )
          : Stream[Mapping]
          = m.parent.toStream flatMap {
              case p : TableMapping => Stream(p)
              case p => p #:: ancestorsToTable(p)
            }

        ancestorsToTable(this)
          .exists{_.isInstanceOf[OptionMapping]}
      }

    lazy val column
      = Column(columnName, columnType, autoIncremented, nullable)

  }