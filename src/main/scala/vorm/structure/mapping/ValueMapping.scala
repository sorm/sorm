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

    // autoIncremented seems to be implemented much smarter
    lazy val isKeyPart
      = {
        val ancestors
          = {
            def ancestors
              ( m : Mapping )
              : Stream[Mapping]
              = m.membership
                  .map{_.parent}
                  .map{
                    case p : EntityMapping ⇒ p #:: Stream.empty
                    case p ⇒ p #:: ancestors(p)
                  }
                  .getOrElse(Stream.empty)
            ancestors(this)
          }

        val containerEntity
          = ancestors.collectFirst{ case m : EntityMapping ⇒ m }

        containerEntity
          .map{ containerEntity ⇒ 

            val containerEntitySettings
              = settingsMap( containerEntity.reflection )

            val keyProperties 
              = ( Set() ++ 
                  containerEntitySettings.primaryKey.view ++
                  containerEntitySettings.uniqueKeys.view.flatten ++
                  containerEntitySettings.indexes.view.flatten )
                  .map{ containerEntity.properties }

            (this #:: ancestors).exists(keyProperties)
          }
          .getOrElse(false)
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

  }