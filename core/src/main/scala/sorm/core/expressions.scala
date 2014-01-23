package sorm.core.expressions

import sorm.core._
import static._
import util._

/**
 * Compiler of static expression templates and processor of associated dynamic values.
 */
abstract class Compiler[-inputTemplate, -inputValues, +outputTemplate, +outputValues] {
  def compileTemplate(input: inputTemplate): outputTemplate
  def processValues(input: inputValues): outputValues
}

/**
 * Templates with complete type-level representation.
 */
// TODO: Get rid of value-level stuff.
// Although, value-level stuff may well be needed for runtime caching of compilation results.
object templates {

  sealed trait Where
  object Where {
    case class Fork
      [ +left <: Where, +right <: Where, +or <: typeLevel.Bool ]
      ( left: left, right: right, or: or )
      extends Where
    case class Comparison
      [ root, +path <: TypePath[root], +operator <: Operator, +negative <: typeLevel.Bool ]
      ( path: path, operator: operator, negative: negative )
      extends Where
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

}

/**
 * Encoding of dynamic values structure associated with template.
 */
object values {

  sealed trait Where
  object Where {
    case class Fork[ +left, +right ]( left: left, right: right ) extends Where
    case class Comparison[ expression <: Expression ]( expression: expression ) extends Where
  }

  sealed trait Expression
  object Expression {
    case class Value[ value ]( value: value ) extends Expression
  }

}

//case class Expression
//  [template <: templates.Template, values <: values.Values]
//  (template: template, values: values)

