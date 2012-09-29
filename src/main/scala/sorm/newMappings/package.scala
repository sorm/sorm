package sorm

import sext.Sext._

import sorm._
import reflection.Reflection
import core._
import scala.Some

package object newMappings {
  def ddlName ( string : String )
    : String
    = {
      import com.google.common.base.CaseFormat._
      UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
    }


  sealed trait Mapping extends Mapping {
    protected lazy val containerTableMapping : Option[TableMapping]
      = membership
          .map{ _.parent }
          .flatMap{
            case parent : TableMapping => Some(parent)
            case parent : Mapping => parent.containerTableMapping
          }
    protected def membership : Option[Membership]
    protected def reflection : Reflection

//    protected def mappings : Stream[Mapping]  //  child mappings
    protected def settings : Map[Reflection, EntitySettings]

    protected def driver : Driver

    protected lazy val memberName : String
//      = membership
//          .map{
//            case Membership.SeqIndex(_) =>
//              "i"
//            case Membership.EntityId(_) =>
//              "id"
//            case Membership.EntityProperty(name, _) =>
//              ddlName(name)
//            case Membership.TupleItem(index, tuple) =>
//              tuple.memberName + "$" + index
//            case Membership.OptionItem(option) =>
//              option.memberName
//            case Membership.SeqItem(_) =>
//              "v"
//            case Membership.SetItem(_) =>
//              "v"
//            case Membership.MapKey(_) =>
//              "k"
//            case Membership.MapValue(_) =>
//              "v"
//            case Membership.RangeFrom(r) =>
//              r.memberName + "$f"
//            case Membership.RangeTo(r) =>
//              r.memberName + "$t"
//          }
//          .getOrElse("")
      = ???
  }
  object Mapping {
    def apply 
      ( reflection : Reflection, 
        membership : Option[Membership], 
        settings : Map[Reflection, EntitySettings],
        driver : Driver )
      : Mapping with Parsing
      = ???
    def apply
      ( reflection : Reflection,
        membership : Membership,
        settings : Map[Reflection, EntitySettings],
        driver : Driver )
      : Mapping with Parsing
      = ???
  }

  import ddl._

  trait CompositeMapping extends Mapping {
    protected def mappings : Stream[Mapping]
    lazy val columns : Stream[Column]
      = mappings flatMap {
          case c : TableMapping => Stream()
          case c : CompositeMapping => c.columns
          case c : ColumnMapping => c.column +: Stream()
        }

    /**
     * First descendant table mappings
     * Or containedTableMappings
     */
    lazy val containedTableMappings : Stream[TableMapping]
      = mappings flatMap {
          case c : ColumnMapping => Stream()
          case c : TableMapping => c +: Stream()
          case c : CompositeMapping => c.containedTableMappings
        }
  }

  
  sealed trait TableMapping extends CompositeMapping {
    
    protected def primaryKey : Stream[String]

    protected lazy val uniqueKeys : Set[Seq[String]] 
      = settings get reflection map (_.uniqueKeys) getOrElse Set()
    protected lazy val indexes : Set[Seq[String]] 
      = settings get reflection map (_.indexes) getOrElse Set()
      
    protected def isMasterTable : Boolean

    protected lazy val masterTableForeignKey : Option[ForeignKey]
      = containerTableMapping
          .flatMap(_.satisfying(_.isMasterTable))
          .map(m => ForeignKey(
            m.name,
            m.primaryKey.map(n => "p$" + n -> n),
            ForeignKey.ReferenceOption.Cascade
          ))

    protected lazy val foreignKeyForContainer : Option[ForeignKey]
      = if( isMasterTable )
          Some(ForeignKey(
            name,
            primaryKey.map(n => memberName + "$" + n -> n),
            ForeignKey.ReferenceOption.Cascade
          ))
        else
          None

    private lazy val containedForeignKeys : Stream[ForeignKey]
      = containedTableMappings flatMap (_.foreignKeyForContainer)
    // lazy val foreignKeyByTable : Map[TableMapping, ForeignKey]
    //   = containedTableMappings
    //       .filter(_.isMasterTable)
    //       .map()

    protected lazy val name : String
      = if (isMasterTable)
          ddlName(reflection.name)
        else
          containerTableMapping.map(_.name + "$").getOrElse("") + memberName

    lazy val table
      = Table(
          name
            = name,
          columns
            = columns,
          primaryKey
            = primaryKey,
          uniqueKeys
            = uniqueKeys,
          indexes
            = indexes,
          foreignKeys
            = masterTableForeignKey ++: containedForeignKeys ++: Set()
        )
  }

  
  // trait MasterTableMapping extends TableMapping {
  //   def name = ddlName(reflection.name)
  // }
  // trait SlaveTableMapping extends TableMapping {
  //   def name = ???
  // }


  trait ColumnMapping extends Mapping {
    def column : Column

  }
}