package sorm.structure.mapping

import sorm._
import extensions.Extensions._
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
      = {
        // def ancestors 
        //   = unfold1 { _.parent }
        // // def ancestors = unfold { _.parent map { p => p -> p } }
        // // def ancestors = parent unfold { p => p -> p flatMap {_.parent} }

        // def ancestorsTillTable 
        //   = ancestors takeWhile {!_.isInstanceOf[TableMapping]}


        // parent unfold {
        //   _ flatMap {
        //     case m : TableMapping => Some(m, None)
        //     case m => Some(m, m.parent)
        //   }
        // }
        // .exists{_.isInstanceOf[OptionMapping]}

        // parent unfold {
        //   case None => None
        //   case Some(m : TableMapping) => Some(m, None)
        //   case Some(m) => Some(m, m.parent)
        // }

        // unfold {
        //   case m : TableMapping => None
        //   case m => Some(m, m.parent)
        // }



        // def ancestorsToTable
        //   ( m : Mapping )
        //   : Stream[Mapping]
        //   = m.parent.toStream flatMap {
        //       case p : TableMapping => Stream(p)
        //       case p => p #:: ancestorsToTable(p)
        //     }

        // ancestorsToTable(this)
        //   .exists{_.isInstanceOf[OptionMapping]}
        
        // parent.iterate( _.flatMap(_.parent) )
        
        ( this : Mapping )
          .unfold1( _.parent )
          .takeWhile( !_.isInstanceOf[TableMapping] )
          .exists( _.isInstanceOf[OptionMapping] )

      }

    lazy val column
      = Column(columnName, columnType, autoIncremented, nullable)

  }