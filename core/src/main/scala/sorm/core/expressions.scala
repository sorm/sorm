package sorm.core.expressions

import sorm.core._
import sorm.core.util._

/**
 * Compiler of static expression templates and processor of associated dynamic values.
 */
trait Compiler[inputTemplate, inputValues, outputTemplate, outputValues] {
  def compileTemplate(input: inputTemplate): outputTemplate
  def processValues(input: inputValues): outputValues
}

/**
 * Templates with complete type-level representation.
 */
object templates {

  sealed trait Where
  object Where {
    case class Fork
      [ left <: Where, right <: Where, or <: typeLevel.Bool ]
      ( left: left, right: right, or: or )
      extends Where
    case class Comparison
      [ path <: TypePath, operator <: Operator, negative <: typeLevel.Bool ]
      ( path: path, operator: operator, negative: negative )
      extends Where
  }

  sealed trait Operator
  object Operator {
    case class Equal extends Operator
    case class Larger extends Operator
    case class Smaller extends Operator
    case class Like extends Operator
    case class Regex extends Operator
    case class In extends Operator
    case class Contains extends Operator
    /**
     * Makes part of a collection
     */
    case class Constitutes extends Operator
    /**
     * Includes a collection
     */
    case class Includes extends Operator
    case class HasSize extends Operator
  }

}

/**
 * Encoding of dynamic values structure associated with template.
 */
object values {

  sealed trait Where
  object Where {
    case class Fork[ left, right ]( left: left, right: right ) extends Where
    case class Comparison[ value <: Expression ]( value: value ) extends Where
  }

  sealed trait Expression
  object Expression {
    case class Value[ value ]( value: value ) extends Expression
  }

}

//case class Expression
//  [template <: templates.Template, values <: values.Values]
//  (template: template, values: values)

