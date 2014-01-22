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
          parent
            .getOrElse(bug("Slave table mapping has no parent"))
            .primaryKeyColumnNames
            .map("p$" + _)
        case _ => Nil
      }
    }
    def child( ref: ChildRef ): Option[Mapping] = {
      ???
    }
    def foreignKeyTo( target: Mapping ): ddl.ForeignKey = {
      val tableName = target.tableName
      val bindings = {
        val nameBasis = nameBasisFor(target)
        target.primaryKeyColumnNames.map(n => (nameBasis + "$" + n, n))
      }
      val onDelete = ddl.ReferenceMode.Cascade
      val onUpdate = ddl.ReferenceMode.NoAction
      ddl.ForeignKey(tableName, bindings, onDelete, onUpdate)
    }
    /**
     * A name for other mapping from the perspective of this mapping.
     */
    def nameBasisFor( target: Mapping ): String = ???
    def tableName: String = ???

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
