package sorm.relational

import reflect.runtime.{universe => ru}
import sorm.core._
import sorm.relational._
import dynamic._

/**
 * Rules for dynamic resolution of inter-type relations.
 */
object rules {

  sealed trait Scenario
  object Scenario {
    case object Primitive extends Scenario
    case object Tuple extends Scenario
    case object CaseClass extends Scenario
    // FIXME: Are we sure we need that kinda distinction?
    case object OptionToTable extends Scenario
    case object OptionToNullable extends Scenario
    case object Seq extends Scenario
    case object Set extends Scenario
    case object Map extends Scenario
    case object Range extends Scenario
    case object Enum extends Scenario

    def fromType( t: ru.Type ): Scenario =
      if( t <:< ru.typeOf[ Product ] )
        if( util.reflection.isTuple(t) ) Tuple
        else if( t.typeSymbol.asClass.isCaseClass ) CaseClass
        else bug("No scenario for product type: " + t)
      else if( ??? ) ???
      else bug("No scenario for type: " + t)
  }

  case class Membership( parent: Mapping, ref: ChildRef )

  class Mapping {
    def t: ru.Type = ???
    def scenario: Scenario = ???
    private def membership: Option[Membership] = ???
    def parent = membership.map(_.parent)
    def ancestors: Stream[Mapping] = parent.map(p => p +: p.ancestors).getOrElse(Stream.empty)
    def primaryKeyColumnNames: Seq[String] = {
      scenario match {
        case Scenario.CaseClass => "id" +: Nil
        case Scenario.OptionToTable | Scenario.Seq | Scenario.Set | Scenario.Map =>
          parent.map(_.primaryKeyColumnNames.map("p$" + _)).getOrElse(Nil)
        case _ => Nil
      }
    }

    def tableName: Option[String] = {
      scenario match {
        case Scenario.CaseClass => Some(ddlEncode(util.reflection.name(t.typeSymbol)))
        case Scenario.OptionToTable | Scenario.Seq | Scenario.Set | Scenario.Map =>
          for {
            parent <- this.parent
            parentName <- parent.tableName
            memberName <- this.memberNameBasis
          }
          yield parentName + "$" + memberName
        case _ => None
      }
    }

    // Propertyish approach
    def memberNameBasis: Option[String] =
      membership.flatMap{ case Membership(parent, ref) =>
        (parent.scenario, ref) match {
          case (Scenario.CaseClass, ChildRef.ByName(name)) => Some(ddlEncode(name))
          case (Scenario.Map, ChildRef.ByIndex(0)) => Some("k")
          case (Scenario.Map, ChildRef.ByIndex(1)) => Some("v")
          case (Scenario.OptionToNullable, ChildRef.ByIndex(0)) => parent.memberNameBasis
          case (Scenario.OptionToTable, ChildRef.ByIndex(0)) => Some("v")
          case (Scenario.Range, ChildRef.ByIndex(0)) => parent.memberNameBasis.map(_ + "$s")
          case (Scenario.Range, ChildRef.ByIndex(1)) => parent.memberNameBasis.map(_ + "$e")
          case _ => None
        }
      }

    /**
     * For slave tables.
     */
    def foreignKeyToParent: Option[ddl.ForeignKey] =
      for {
        parent <- this.parent
        tableName <- parent.tableName
        bindings = parent.primaryKeyColumnNames.map(n => ("p$" + n, n))
        onDelete = ddl.ReferenceMode.Cascade
        onUpdate = ddl.ReferenceMode.NoAction
      }
      yield ddl.ForeignKey(tableName, bindings, onDelete, onUpdate)

    /**
     * A foreign key for a parent table.
     */
    def foreignKeyForParent: Option[ddl.ForeignKey] =
      for {
        tableName <- this.tableName
        bindings = primaryKeyColumnNames.map(n => (memberNameBasis + "$" + n, n))
        onDelete = ddl.ReferenceMode.Cascade
        onUpdate = ddl.ReferenceMode.NoAction
      }
      yield ddl.ForeignKey(tableName, bindings, onDelete, onUpdate)

  }

  trait MappingResolver[ path ] {
    def mapping: Mapping
  }
  object MappingResolver {
    implicit def propertyInstance
      [ root, parent <: static.TypePath[ root ], index <: shapeless.Nat ]
      =
      new MappingResolver[ static.TypePath.Property[root, parent, index] ] {
        def mapping = ???
      }
  }

  private def ddlEncode ( string : String ) : String = {
    import com.google.common.base.CaseFormat._
    UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
  }


}
