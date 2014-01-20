package sorm.relational

import reflect.runtime.{universe => ru}
import sorm.core._
import sorm.relational._
import mapping._
import runtime._

/**
 * Rules for dynamic resolution of inter-type relations.
 */
object rules {

  def columnName( t: ru.Type ): String = ???

  def bindingsToParent( rep: mapping.Rep ): Seq[(String, String)] = rep match {
    case mapping.Rep.CaseClass(properties) => ???
    case _ => ???
  }

  // def bindingsToParent
  //   ( implicit )
  //   =

  def bindingsToParent( member: Member ): Seq[(String, String)] = member match {
    case member: Member.Root => Seq.empty
    case member: Member.Generic =>
      val t = memberType(member)
      val parentT = memberType(member.parent)
      val scenario = Scenario.fromType(t)
      val parentScenario = Scenario.fromType(parentT)
      ???
  }

  def foreignKeyForParent( member: Member ): Option[ddl.ForeignKey] = member match {
    case member: Member.Root => None
    case member: Member.Property =>
      val t = memberType(member)
      val parentT = memberType(member.parent)
      ???
  }

  // def bindingsToParent( rootType: ru.Type, path: DynamicTypePath ) = 
  //   path match {
  //     case DynamicTypePath.Root => Seq.empty
  //     case DynamicTypePath.MemberByName(parent, name) => {
  //       val t = typeAtPath(rootType, path)
  //       val scenario = Scenario.fromType(t)

  //     }
  //   }
  
  /**
   * Resolve bindings to a direct child of a type.
   */
  def bindingsToChild( t: ru.Type, ref: ChildRef ) = {
    val childType = runtime.childType(t, ref)
    val childPrimaryKey = ???
    ref match {
      case ChildRef.ByName(name) => {
        ???
      }
    }
  }

  class Mapping {
    def scenario: Scenario = ???
    def parent: Option[Mapping] = ???
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
    def foreignKeyToChild( child: Mapping ): ddl.ForeignKey = {
      val tableName = child.tableName
      val bindings = {
        val childName = child.memberName
        child.primaryKeyColumnNames.map(n => (childName + "$" + n, n))
      }
      val onDelete = ddl.ReferenceMode.Cascade
      val onUpdate = ddl.ReferenceMode.NoAction
      ddl.ForeignKey(tableName, bindings, onDelete, onUpdate)
    }
    def tableName: String = ???
    def memberName: String = {
      ???
    }
  }


}
