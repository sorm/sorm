package sorm.newMappings

import sext.Sext._

import sorm._
import reflection._
import ddl._
import org.joda.time._

trait ColumnMapping extends Mapping {
  lazy val column
    = Column(memberName, columnType, autoIncremented, nullable)
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
                e.uniqueKeys.view.flatten.exists(_ == n) ||
                e.indexes.view.flatten.exists(_ == n)
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

  lazy val nullable
    = ( this : Mapping )
        .unfold1( _.membership.map(_.parent) )
        .takeWhile( !_.isInstanceOf[TableMapping] )
        .exists( _.isInstanceOf[OptionMapping] )

  override def valueFromContainerRow(data: String => Any) = data(memberName)

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

  lazy val autoIncremented : Boolean
    = membership match {
        case Some(Membership.EntityId(_)) => true
        case _ => false
      }

}