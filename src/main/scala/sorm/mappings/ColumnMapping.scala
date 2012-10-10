package sorm.mappings

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

  lazy val nullable = false

  def columnType : ColumnType

  lazy val autoIncremented : Boolean
    = membership match {
        case Some(Membership.EntityId(_)) => true
        case _ => false
      }

}