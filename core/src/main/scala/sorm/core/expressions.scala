package sorm.core.expressions

import sorm._, core._, util._, static._

/**
 * Templates with complete type-level representation.
 */
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
    case class Select[ +select <: templates.SelectSpec ]( select: select ) extends Action
    case class Update[ +select <: templates.SelectSpec ]( select: select ) extends Action
    case class Delete[ +select <: templates.SelectSpec ]( select: select ) extends Action
    case class Insert() extends Action
  }

  sealed trait SelectSpec
  object SelectSpec {
    case class From() extends SelectSpec
    case class Limit[ tail <: SelectSpec ]( tail: tail ) extends SelectSpec
    case class Offset[ tail <: SelectSpec ]( tail: tail ) extends SelectSpec
    case class OrderBy[ path <: TypePath[_], desc <: typeLevel.Bool, tail <: SelectSpec ]
                      ( path: path, desc: desc, tail: tail ) extends SelectSpec
    case class Where[ condition <: Condition, tail <: SelectSpec ]
                    ( condition: condition, tail: tail ) extends SelectSpec

  }

}

/**
 * Encoding of dynamic values structure associated with template.
 */
object values {

  sealed trait Condition
  object Condition {
    case class Fork[ +left, +right ]( left: left, right: right ) extends Condition
    case class Comparison[ value ]( value: value ) extends Condition
  }

  sealed trait Action
  object Action {
    case class Select[ spec <: values.SelectSpec ]( spec: spec ) extends Action
  }

  sealed trait SelectSpec
  object SelectSpec {
    case class From() extends SelectSpec
    case class Limit[ tail <: SelectSpec ]( by: Int, tail: tail ) extends SelectSpec
    case class Offset[ tail <: SelectSpec ]( by: Int, tail: tail ) extends SelectSpec
  }

}
