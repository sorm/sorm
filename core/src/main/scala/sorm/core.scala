package sorm; package object core {


import reflect.runtime.universe._


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
  ( contextType : Type,
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

/**
 * Exports of this module to be mixed into the public API, e.g. `sorm._`.
 */
trait Exports {
  import language.experimental.macros

  type Persisted = api.Persisted
  type Entity[a] = api.Entity[a]
  type Key[entity, fields] = api.Key[entity, fields]
  val Key = api.Key

  def entity[ a ]
    ( keys : Set[ Key[ a, Any ] ] )
    : Entity[ a ]
    = macro api.Macros.entity[ a ]

  def uniqueKey[ entity, fields ]( f : entity => fields )
    : Key.Unique[ entity, fields ]
    = macro api.Macros.uniqueKey[ entity, fields ]

}

}
