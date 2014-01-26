package sorm.core.expressions

import sorm._, core._, util._, static._

/**
 * Templates with complete type-level representation.
 */
// TODO: Get rid of value-level stuff.
// Although, value-level stuff may well be needed for runtime caching of compilation results.
object templates {

  sealed trait Condition
  object Condition {
    case class Fork
      [ +left <: Condition, +right <: Condition, +or <: typeLevel.Bool ]
      ( left: left, right: right, or: or )
      extends Condition
    case class Comparison
      [ root, +path <: TypePath[root], +operator <: Operator, +negative <: typeLevel.Bool ]
      ( path: path, operator: operator, negative: negative )
      extends Condition
  }

  sealed trait Operator
  object Operator {
    sealed trait Equal extends Operator; case object Equal extends Equal
    sealed trait Larger extends Operator; case object Larger extends Larger
    sealed trait Smaller extends Operator; case object Smaller extends Smaller
    sealed trait Like extends Operator; case object Like extends Like
    sealed trait Regex extends Operator; case object Regex extends Regex
    sealed trait In extends Operator; case object In extends In
    sealed trait Contains extends Operator; case object Contains extends Contains
    /**
     * Makes part of a collection
     */
    sealed trait Constitutes extends Operator; case object Constitutes extends Constitutes
    /**
     * Includes a collection
     */
    sealed trait Includes extends Operator; case object Includes extends Includes
    sealed trait HasSize extends Operator; case object HasSize extends HasSize
  }

  sealed trait Action
  object Action {
    case class Select[ select <: templates.Select ]( select: select ) extends Action
    case class Update[ select <: templates.Select ]( select: select ) extends Action
    case class Delete[ select <: templates.Select ]( select: select ) extends Action
    case class Insert[ root ] extends Action
  }

  sealed trait Select
  object Select {
    case class From
      [ root ] 
      extends Select
    case class Limit
      [ tail <: Select ]
      ( tail: tail )
      extends Select
    case class Offset
      [ tail <: Select ]
      ( tail: tail )
      extends Select
    case class OrderBy
      [ path <: TypePath[_], desc <: typeLevel.Bool, tail <: Select ]
      ( path: path, desc: desc, tail: tail )
      extends Select
    case class Where
      [ condition <: Condition, tail <: Select ]
      ( condition: condition, tail: tail )
      extends Select

    trait RootResolver[template] {
      type Root
    }
    object RootResolver {
      implicit def from[ root ] = new RootResolver[ From[root] ]{ type Root = root }
      implicit def limit
        [ tail <: Select ]
        ( implicit tailResolver: RootResolver[tail] )
        = new RootResolver[ Limit[tail] ]{ type Root = tailResolver.Root }
        
    }
  }

}

/**
 * Encoding of dynamic values structure associated with template.
 */
object values {

  sealed trait Condition
  object Condition {
    case class Fork[ +left, +right ]( left: left, right: right ) extends Condition
    case class Comparison[ expression <: Expression ]( expression: expression ) extends Condition
  }

  sealed trait Expression
  object Expression {
    case class Value[ value ]( value: value ) extends Expression
  }

}

trait Runner {
  def run
    [ template <: templates.Action ]
    ( template: template, values: Seq[Any] )
    ( implicit parser: Parser[ template ] )
    : parser.Result
}
