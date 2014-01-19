package sorm.relational.mapping

import reflect.runtime.{universe => ru}
import sorm.core._

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
    else bug("No scenario for type: " + t)
}

/**
 * A runtime representation of a type in a position relative to a root entity.
 */
sealed trait Rep
object Rep {
  case class CaseClass( properties: Seq[ru.TermSymbol] ) extends Rep
}

trait RepResolver[ path ] {
  def rep: Rep
}
object RepResolver {
  def propertyInstance
    [ root, parent <: TypePath[root], index <: shapeless.Nat ]
    ( implicit 
        typeResolver: TypeResolver[ TypePath.Property[ root, parent, index ] ] )
    =
    new RepResolver[ TypePath.Property[ root, parent, index ] ] {
      val rep = {
        val t = typeResolver.head
        val scenario = Scenario.fromType(t)
        scenario match {
          case Scenario.Primitive => ???
          case _ => ???
        }

      }
    }
}

