package sorm.structure.mapping

import sorm._
import sext.Sext._
import reflection._
import ddl._
import structure._

trait ColumnMapping 
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

    def columnType : Column.Type

    def autoIncremented : Boolean
    
    lazy val nullable
      = ( this : Mapping )
          .unfold1( _.parent )
          .takeWhile( !_.isInstanceOf[TableMapping] )
          .exists( _.isInstanceOf[OptionMapping] )

    lazy val column
      = Column(columnName, columnType, autoIncremented, nullable)

  }