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
    // TODO: Add more specific types
    case object Int extends Scenario
    case object Long extends Scenario
    case object BigDecimal extends Scenario
    case object Boolean extends Scenario
    case object StringToVarchar extends Scenario
    case object StringToClob extends Scenario
  }

  case class Membership( parent: Mapping, ref: ChildRef )

  class Mapping(val t: ru.Type, val membership: Option[Membership]) {
    val child = util.memo{ ref: ChildRef =>
      val childType = helpers.childType(t, ref)
      val membership = Membership(this, ref)
      new Mapping(childType, Some(membership))
    }
    def scenario: Scenario = {
      def is[ t : ru.TypeTag ] = t <:< ru.typeOf[t]
      if( is[ Product ] )
        if( util.reflection.isTuple(t) ) Scenario.Tuple
        else if( t.typeSymbol.asClass.isCaseClass ) Scenario.CaseClass
        else bug("No scenario for product type: " + t)
      else if( is[ Int ] ) Scenario.Int
      else if( is[ Long ] ) Scenario.Long
      else if( is[ BigDecimal ] ) Scenario.BigDecimal
      else if( is[ Boolean ] ) Scenario.Boolean
      else if( is[ String ] ) bug("String scenario detection unimplemented")
      else if( ??? ) ???
      else bug("No scenario for type: " + t)
    }
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

    // TODO: reimplement it based on a typeclass, since drivers may perform different mapping.
    def columnType: Option[model.ColumnType] = {
      val s = Scenario
      val ct = model.ColumnType
      val partial: PartialFunction[Scenario, model.ColumnType] = {
        case s.Int => ct.Integer
        case s.Long => ct.BigInt
        case s.Boolean => ct.TinyInt
        case _ => ???
      }
      partial.lift.apply(scenario)
    }
    def jdbcType: Option[JDBCType] = columnType.map(model.ColumnType.jdbcType)

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
    def foreignKeyToParent: Option[model.ForeignKey] =
      for {
        parent <- this.parent
        tableName <- parent.tableName
        bindings = parent.primaryKeyColumnNames.map(n => ("p$" + n, n))
        onDelete = model.ReferenceMode.Cascade
        onUpdate = model.ReferenceMode.NoAction
      }
      yield model.ForeignKey(tableName, bindings, onDelete, onUpdate)

    /**
     * A foreign key for a parent table.
     */
    def foreignKeyForParent: Option[model.ForeignKey] =
      for {
        tableName <- this.tableName
        bindings = primaryKeyColumnNames.map(n => (memberNameBasis + "$" + n, n))
        onDelete = model.ReferenceMode.Cascade
        onUpdate = model.ReferenceMode.NoAction
      }
      yield model.ForeignKey(tableName, bindings, onDelete, onUpdate)

  }

  trait MappingResolver[ path ] {
    val mapping: Mapping
  }
  object MappingResolver {
    implicit def genericInstance
      [ root, parent <: static.TypePath[ root ], index <: shapeless.Nat ]
      ( implicit
          parentMappingResolver: MappingResolver[parent],
          childRefResolver: ChildRefResolver[static.TypePath.Generic[root, parent, index]] )
      =
      new MappingResolver[ static.TypePath.Generic[root, parent, index] ] {
        val mapping = {
          val parentMapping = parentMappingResolver.mapping
          val childRef = childRefResolver.childRef
          parentMapping.child(childRef)
        }
      }
    implicit def propertyInstance
      [ root, parent <: static.TypePath[ root ], index <: shapeless.Nat ]
      ( implicit
          parentMappingResolver: MappingResolver[parent],
          childRefResolver: ChildRefResolver[static.TypePath.Property[root, parent, index]] )
      =
      new MappingResolver[ static.TypePath.Property[root, parent, index] ] {
        val mapping = {
          val parentMapping = parentMappingResolver.mapping
          val childRef = childRefResolver.childRef
          parentMapping.child(childRef)
        }
      }
    implicit def rootInstance
      [ root ]
      ( implicit rootTypeResolver: static.TypeResolver[ static.TypePath.Root[root] ] )
      =
      new MappingResolver[ static.TypePath.Root[root] ] {
        val mapping = {
          val t = rootTypeResolver.head
          new Mapping(t, None)
        }
      }
  }

  private def ddlEncode ( string : String ) : String = {
    import com.google.common.base.CaseFormat._
    UPPER_CAMEL.to( LOWER_UNDERSCORE, string )
  }


}
