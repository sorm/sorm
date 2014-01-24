package sorm
package object core {

import scala.reflect.runtime.{universe => ru}


def bug ( m : String ) = sys.error("A SORM bug appeared. Please, report the following message to maintainers: " + m)
def todo ( m : String ) = sys.error("Reached an unimplemenented SORM feature. Please, report the following message to maintainers: " + m)

/**
 * Compiler of expression templates and arranger of associated values.
 */
//  TODO: Probably, better use lambdas.
trait Compiler[-inputTemplate, -inputValues, +outputTemplate, +outputValues] {
  def renderTemplate(input: inputTemplate): outputTemplate
  def arrangeValues(input: inputValues): outputValues
}


/**
 * A reference from a context type to its subfield. E.g., `context.field` or
 * `context.field.field` and so on.
 *
 * @tparam source A type of the context object.
 * @tparam target A type of the referred end-value.
 * @param contextType A type object of the context object.
 * @param subFieldSymbols A list of symbols representing the chain of subfields.
 *
 * @example
 *   For example, a reference `a.b.c` should be represented as follows:
 *   {{{
 *   SubRef
 *     [ <Type of `a`>, <Type of `c`> ]
 *     ( <rep of type of `a`>, List( <Symbol of field `b` of type of `a`>,
 *                                   <Symbol of field `c` of type of `a.b`> ) )
 *   }}}
 */
case class FieldRef
  [ source, target ]
  ( contextType : ru.Type,
    subFieldSymbols : List[ Symbol ] )

sealed trait FieldRefs
  [ source, targets ]

object FieldRefs {
  case class FieldRefsValue
    [ source, targetsHead, targetsTail ]
    ( subRef : FieldRef[ source, targetsHead ],
      tail : FieldRefs[ source, targetsTail ] )
    extends FieldRefs[ source, (targetsHead, targetsTail) ]

  case class FieldRefsNil[ a ] extends FieldRefs[ a, Unit ]
}



}